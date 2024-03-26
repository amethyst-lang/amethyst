use std::collections::{HashMap, hash_map::Entry};

use crate::ast::*;

#[derive(Debug)]
struct TypeData {
    va_params: bool,
    params: Vec<String>,
    variants: Vec<Variant>,
}

struct GlobalData {
    type_: Type,
    variant_of: Option<String>,
}

pub struct TypeError {
    pub message: String,
}

pub fn typecheck(ast: &[TopLevel]) -> Result<(), TypeError> {
    // set up scopes
    let mut globals = HashMap::new();
    let mut defined_types = HashMap::new();
    let mut type_vars = Vec::new();
    defined_types.insert("Tuple".to_owned(), TypeData {
        va_params: true,
        params: Vec::new(),
        variants: Vec::new(),
    });
    defined_types.insert("Int".to_owned(), TypeData {
        va_params: false,
        params: Vec::new(),
        variants: Vec::new(),
    });
    defined_types.insert("Unit".to_owned(), TypeData {
        va_params: false,
        params: Vec::new(),
        variants: Vec::new(),
    });
    defined_types.insert("String".to_owned(), TypeData {
        va_params: false,
        params: Vec::new(),
        variants: Vec::new(),
    });
    defined_types.insert("Pointer".to_owned(), TypeData {
        va_params: false,
        params: vec!["T".to_string()],
        variants: Vec::new(),
    });

    // get all globals (functions and type definitions)
    for top in ast {
        match top {
            TopLevel::TypeDef { name, generics, variants } => {
                let Entry::Vacant(v) = defined_types.entry(name.clone())
                else {
                    return Err(TypeError {
                        message: format!("redefinition of {name}"),
                    });
                };

                let self_type = if generics.is_empty() {
                    Type::Name(name.clone())
                } else {
                    Type::App(
                        Box::new(Type::Name(name.clone())),
                        generics.iter().map(|g| Type::Generic(g.clone())).collect())
                };

                for variant in variants {
                    let Entry::Vacant(v) = globals.entry(variant.name.clone())
                    else {
                        return Err(TypeError {
                            message: format!("redefinition of {}", variant.name),
                        });
                    };

                    v.insert(GlobalData {
                        type_: Type::Func(
                            variant.fields.clone(),
                            Box::new(self_type.clone())),
                        variant_of: Some(name.clone()),
                    });
                }

                let data = TypeData {
                    va_params: false,
                    params: generics.clone(),
                    variants: variants.clone(),
                };
                v.insert(data);
            }

            TopLevel::FuncDef { name, args, ret, .. } => {
                let Entry::Vacant(v) = globals.entry(name.clone())
                else {
                    return Err(TypeError {
                        message: format!("redefinition of {name}"),
                    });
                };

                let fn_args: Vec<_> = args.iter().map(|(_, t)| t.clone()).collect();
                v.insert(GlobalData {
                    type_: Type::Func(fn_args, Box::new(ret.clone())),
                    variant_of: None,
                });
            }
        }
    }

    // verify types in globals (type variants are globals so this is sufficient)
    for (_, data) in globals.iter() {
        verify_type(&data.type_, &defined_types)?;
    }

    // typecheck statements
    for top in ast {
        let TopLevel::FuncDef { name, args, ret, stats } = top
        else {
            continue;
        };

        let mut scopes = vec![HashMap::new()];
        for (name, ty) in args {
            scopes[0].insert(name.clone(), ty.clone());
        }

        let mut has_ret = false;
        for stat in stats {
            has_ret |= typecheck_stat(&mut type_vars, &defined_types, &globals, &mut scopes, stat, &ret, false)?;
        }

        match ret {
            Type::Name(n) if n == "Unit" => (),
            _ if !has_ret => return Err(TypeError {
                message: format!("function {name} must terminate with return")
            }),
            _ => (),
        }

        for i in 0..type_vars.len() {
            if is_typevar_unset(&type_vars, i) {
                return Err(TypeError {
                    message: format!("type variable {i} remains unset after type checking {name}"),
                });
            }
        }
    }

    Ok(())
}

fn typecheck_stat(
    type_vars: &mut Vec<Type>,
    defined_types: &HashMap<String, TypeData>,
    globals: &HashMap<String, GlobalData>,
    scopes: &mut Vec<HashMap<String, Type>>,
    stat: &Statement,
    expected_ret: &Type,
    in_loop: bool,
) -> Result<bool, TypeError> {
    match stat {
        Statement::FuncCall { func, args } => {
            let mut a = Vec::new();
            for arg in args {
                a.push(typecheck_expr(type_vars, globals, scopes, arg)?);
            }

            let f = lookup(type_vars, globals, scopes, func)?;
            let ret = valid_call(type_vars, &f, &a)?;
            unify(type_vars, ret, &Type::Name("Unit".to_string()))?;
            Ok(false)
        }

        Statement::Let { name, value } => {
            let ty = typecheck_expr(type_vars, globals, scopes, value)?;
            scopes.last_mut().unwrap().insert(name.clone(), ty);
            Ok(false)
        }

        Statement::Set { name, value } => {
            let ty = typecheck_expr(type_vars, globals, scopes, value)?;
            let local = &lookup_local(type_vars, scopes, name)?;
            unify(type_vars, local, &ty)?;
            Ok(false)
        }

        Statement::Loop { body } => {
            scopes.push(HashMap::new());
            for stat in body {
                typecheck_stat(type_vars, defined_types, globals, scopes, stat, expected_ret, true)?;
            }

            scopes.pop();
            Ok(false)
        }

        Statement::Break | Statement::Continue => {
            if in_loop {
                Ok(false)
            } else {
                Err(TypeError {
                    message: "tried to use loop control flow outside of a loop".to_string(),
                })
            }
        }

        Statement::Return(v) => {
            let ty = match v {
                Some(v) => typecheck_expr(type_vars, globals, scopes, &v)?,
                None => Type::Name("Unit".to_owned()),
            };
            unify(type_vars, &ty, expected_ret)?;
            Ok(true)
        }

        Statement::If { cond, then, elsy } => {
            let ty = typecheck_expr(type_vars, globals, scopes, cond)?;
            match ty.func_of_app() {
                Type::Name(n) => {
                    let Some(data) = defined_types.get(n)
                    else {
                        return Err(TypeError {
                            message: format!("type {n} was not exist"),
                        });
                    };

                    if data.variants.len() != 2
                        || !data.variants[0].fields.is_empty()
                        || !data.variants[1].fields.is_empty()
                    {
                        return Err(TypeError {
                            message: format!("type {n} is incompatible with if"),
                        });
                    }

                    scopes.push(HashMap::new());
                    let mut then_ret = false;
                    for stat in then.iter() {
                        then_ret |= typecheck_stat(type_vars, defined_types, globals, scopes, stat, expected_ret, in_loop)?;
                    }
                    scopes.pop();

                    scopes.push(HashMap::new());
                    let mut else_ret = false;
                    for stat in elsy.iter() {
                        else_ret |= typecheck_stat(type_vars, defined_types, globals, scopes, stat, expected_ret, in_loop)?;
                    }
                    scopes.pop();

                    Ok(then_ret && else_ret)
                }

                _ => Err(TypeError {
                    message: format!("cannot use if on type {ty}"),
                })
            }
        }

        Statement::Match { value, branches } => {
            let t = typecheck_expr(type_vars, globals, scopes, value)?;

            let mut ret = !branches.is_empty();
            for (pat, stats) in branches {
                let mut map = HashMap::new();
                let tp = typecheck_pat(type_vars, globals, defined_types, &mut map, pat)?;
                unify(type_vars, &t, &tp)?;
                scopes.push(map);

                let mut branch_ret = false;
                for stat in stats {
                    branch_ret |= typecheck_stat(type_vars, defined_types, globals, scopes, stat, expected_ret, in_loop)?;
                }

                ret &= branch_ret;
                scopes.pop();
            }

            Ok(ret)
        }
    }
}

fn typecheck_pat(
    type_vars: &mut Vec<Type>,
    globals: &HashMap<String, GlobalData>,
    defined_types: &HashMap<String, TypeData>,
    map: &mut HashMap<String, Type>,
    pat: &Pattern
) -> Result<Type, TypeError> {
    match pat {
        Pattern::Wildcard => Ok(create_typevar(type_vars)),

        Pattern::Symbol(x) => {
            let t = create_typevar(type_vars);
            map.insert(x.clone(), t.clone());
            Ok(t)
        }

        Pattern::Variant { name, args, exhaustive } => {
            let Some(type_name) = globals.get(name)
                .and_then(|v| v.variant_of.as_ref())
            else {
                return Err(TypeError {
                    message: format!("{name} is not a variant of a type"),
                });
            };

            let Some(type_data) = defined_types.get(type_name)
            else {
                unreachable!();
            };

            let t = if type_data.params.is_empty() {
                Type::Name(type_name.clone())
            } else {
                Type::App(
                    Box::new(Type::Name(type_name.clone())),
                    type_data.params.iter().map(|g| Type::Generic(g.clone())).collect(),
                )
            };

            let mut generics_map = HashMap::new();
            let t = instantiate_generics_with_map(&mut generics_map, type_vars, &t);

            let Some(variant) = type_data.variants.iter().find(|v| v.name == *name)
            else {
                unreachable!();
            };

            let exhaustive = *exhaustive;
            if exhaustive && variant.fields.len() != args.len() {
                return Err(TypeError {
                    message: format!("variant {name} from {type_name} is given the incorrect arguments in a pattern"),
                });
            }

            if !exhaustive && variant.fields.len() < args.len() {
                return Err(TypeError {
                    message: format!("variant {name} from {type_name} is given too many arguments in a pattern"),
                });
            }

            for (p, t) in args.iter().zip(variant.fields.iter()) {
                let t = instantiate_generics_with_map(&mut generics_map.clone(), type_vars, &t);
                let t_pat = typecheck_pat(type_vars, globals, defined_types, map, p)?;
                unify(type_vars, &t, &t_pat)?;
            }

            Ok(t)
        }

        Pattern::Or(pats) => {
            let mut t = create_typevar(type_vars);
            let mut first = true;
            let mut temp_map = HashMap::new();

            for pat in pats {
                let mut m = HashMap::new();
                let tp = typecheck_pat(type_vars, globals, defined_types, &mut m, pat)?;
                t = unify(type_vars, &t, &tp)?;

                if first {
                    temp_map = m;
                    first = false;
                } else {
                    if temp_map.len() != m.len() {
                        return Err(TypeError {
                            message: format!("environments from patterns in or pattern are different")
                        });
                    }

                    for (x, t) in m {
                        if let Some(t2) = temp_map.get(&x) {
                            unify(type_vars, &t, t2)?;
                        } else {
                            return Err(TypeError {
                                message: format!("variable {x} found in one pattern but not in another in or pattern"),
                            });
                        }
                    }
                }
            }

            map.extend(temp_map);
            Ok(t)
        }
    }
}

fn typecheck_expr(
    type_vars: &mut Vec<Type>,
    globals: &HashMap<String, GlobalData>,
    scopes: &mut Vec<HashMap<String, Type>>,
    expr: &Expr,
) -> Result<Type, TypeError> {
    match expr {
        Expr::Integer(_) => Ok(Type::Name("Int".to_string())),
        Expr::String(_) => Ok(Type::Name("String".to_string())),
        Expr::Symbol(s) => lookup(type_vars, globals, scopes, s),

        Expr::FuncCall { func, args } => {
            let f = typecheck_expr(type_vars, globals, scopes, &func)?;
            let mut a = Vec::new();
            for a_orig in args {
                a.push(typecheck_expr(type_vars, globals, scopes, a_orig)?)
            }

            valid_call(type_vars, &f, &a).cloned()
        }
    }
}

#[allow(unused)]
fn lookup_global<'a>(
    type_vars: &mut Vec<Type>,
    globals: &'a HashMap<String, GlobalData>,
    ident: &str,
) -> Result<Type, TypeError> {
    if let Some(data) = globals.get(ident) {
        Ok(instantiate_generics(type_vars, &data.type_))
    } else {
        Err(TypeError {
            message: format!("global {ident} does not exist"),
        })
    }
}

fn lookup_local<'a>(
    type_vars: &mut Vec<Type>,
    scopes: &'a Vec<HashMap<String, Type>>,
    ident: &str,
) -> Result<Type, TypeError> {
    for scope in scopes.iter().rev() {
        if let Some(t) = scope.get(ident) {
            return Ok(instantiate_generics(type_vars, t));
        }
    }

    Err(TypeError {
        message: format!("identifier {ident} was never defined locally"),
    })
}

fn lookup<'a>(
    type_vars: &mut Vec<Type>,
    globals: &'a HashMap<String, GlobalData>,
    scopes: &'a Vec<HashMap<String, Type>>,
    ident: &str,
) -> Result<Type, TypeError> {
    for scope in scopes.iter().rev() {
        if let Some(t) = scope.get(ident) {
            return Ok(instantiate_generics(type_vars, t));
        }
    }

    if let Some(data) = globals.get(ident) {
        Ok(instantiate_generics(type_vars, &data.type_))
    } else {
        Err(TypeError {
            message: format!("identifier {ident} was never defined"),
        })
    }
}

fn create_typevar(type_vars: &mut Vec<Type>) -> Type {
    let v = type_vars.len();
    type_vars.push(Type::Typevar(v));
    Type::Typevar(v)
}

fn instantiate_generics_with_map(
    generics: &mut HashMap<String, Type>,
    type_vars: &mut Vec<Type>,
    t: &Type,
) -> Type {
    match t {
        Type::Generic(g) => {
            match generics.entry(g.clone()) {
                Entry::Occupied(v) => v.get().clone(),
                Entry::Vacant(v) => {
                    let t = create_typevar(type_vars);
                    v.insert(t.clone());
                    t
                }
            }
        }

        Type::Func(a, r) => Type::Func(
            a.iter().map(|t| instantiate_generics_with_map(generics, type_vars, t)).collect(),
            Box::new(instantiate_generics_with_map(generics, type_vars, r)),
        ),

        Type::App(f, a) => Type::App(
            Box::new(instantiate_generics_with_map(generics, type_vars, f)),
            a.iter().map(|t| instantiate_generics_with_map(generics, type_vars, t)).collect()
        ),

        _ => t.clone(),
    }
}

fn instantiate_generics(
    type_vars: &mut Vec<Type>,
    t: &Type,
) -> Type {
    instantiate_generics_with_map(&mut HashMap::new(), type_vars, t)
}

fn verify_type(t: &Type, defined_types: &HashMap<String, TypeData>) -> Result<(), TypeError> {
    fn helper(t: &Type, defined_types: &HashMap<String, TypeData>) -> Result<Option<usize>, TypeError> {
        match t {
            Type::Typevar(_) | Type::Generic(_) => Ok(None),

            Type::Name(n) => {
                let Some(data) = defined_types.get(n)
                else {
                    return Err(TypeError {
                        message: format!("type {t} was never defined"),
                    });
                };

                if data.va_params {
                    Ok(None)
                } else {
                    Ok(Some(data.params.len()))
                }
            }

            Type::Func(args, ret) => {
                for arg in args {
                    let (Some(0) | None) = helper(arg, defined_types)?
                    else {
                        return Err(TypeError {
                            message: format!("function parameter {arg} should have 0 expected type parameters"),
                        });
                    };
                }

                let (Some(0) | None) = helper(ret, defined_types)?
                else {
                    return Err(TypeError {
                        message: format!("function return {ret} should have 0 expected type parameters"),
                    });
                };

                Ok(Some(0))
            }

            Type::App(f, a) => {
                for a in a {
                    let (Some(0) | None) = helper(a, defined_types)?
                    else {
                        return Err(TypeError {
                            message: format!("type parameter {a} should have 0 expected type parameters"),
                        });
                    };
                }

                let Some(expected_count) = helper(f, defined_types)?
                else {
                    return Ok(Some(0))
                };

                if expected_count != a.len() {
                    return Err(TypeError {
                        message: format!("expected {expected_count} type parameters for type {t}, got {}", a.len()),
                    });
                }

                Ok(Some(0))
            }
        }
    }

    if let Some(0) | None = helper(t, defined_types)? {
        Ok(())
    } else {
        Err(TypeError {
            message: format!("type {t} should expect 0 type parameters"),
        })
    }
}

fn valid_call<'a>(
    type_vars: &mut Vec<Type>,
    f: &'a Type,
    args: &[Type],
) -> Result<&'a Type, TypeError> {
    match f {
        Type::Func(params, ret) => {
            if args.len() != params.len() {
                return Err(TypeError {
                    message: format!("passed in {} arguments when expected {}", args.len(), params.len()),
                });
            }

            for (a, t) in args.iter().zip(params.iter()) {
                unify(type_vars, a, t)?;
            }
            Ok(&ret)
        }

        _ => Err(TypeError {
            message: format!("type {f} is not callable"),
        })
    }
}

fn get_typevar_index(
    type_vars: &Vec<Type>,
    mut i: usize,
) -> usize {
    loop {
        let new_t = &type_vars[i];
        match new_t {
            &Type::Typevar(j) if i == j => return i,
            &Type::Typevar(j) => i = j,
            _ => return i,
        }
    }
}

fn is_typevar_unset(
    type_vars: &Vec<Type>,
    v: usize,
) -> bool {
    matches!(type_vars[v], Type::Typevar(i) if i == v)
}

fn unify(
    type_vars: &mut Vec<Type>,
    t1: &Type,
    t2: &Type,
) -> Result<Type, TypeError> {
    match (t1, t2) {
        (&Type::Typevar(v1), &Type::Typevar(v2)) => {
            let v1 = get_typevar_index(type_vars, v1);
            let v2 = get_typevar_index(type_vars, v2);

            if is_typevar_unset(type_vars, v1) {
                type_vars[v1] = type_vars[v2].clone();
                Ok(type_vars[v2].clone())
            } else if is_typevar_unset(type_vars, v2) {
                type_vars[v2] = type_vars[v1].clone();
                Ok(type_vars[v1].clone())
            } else {
                unify(type_vars, &type_vars[v1].clone(), &type_vars[v2].clone())
            }
        }

        (&Type::Typevar(v), t) | (t, &Type::Typevar(v)) => {
            let v = get_typevar_index(type_vars, v);
            if is_typevar_unset(type_vars, v) {
                type_vars[v] = t.clone();
                Ok(t.clone())
            } else {
                let t = unify(type_vars, &type_vars[v].clone(), t)?;
                type_vars[v] = t.clone();
                Ok(t)
            }
        }

        (Type::Name(n1), Type::Name(n2)) if n1 == n2 => Ok(t1.clone()),

        (Type::Func(a1, r1), Type::Func(a2, r2)) => {
            if a1.len() != a2.len() {
                return Err(TypeError {
                    message: format!("types {} and {} cannot be unified", t1, t2),
                });
            }

            let mut a = Vec::new();
            for (a1, a2) in a1.iter().zip(a2.iter()) {
                a.push(unify(type_vars, a1, a2)?);
            }

            let r = unify(type_vars, r1, r2)?;

            Ok(Type::Func(a, Box::new(r)))
        }

        (Type::App(f1, a1), Type::App(f2, a2)) => {
            let f = unify(type_vars, f1, f2)?;

            if a1.len() != a2.len() {
                return Err(TypeError {
                    message: format!("types {} and {} cannot be unified", t1, t2),
                });
            }

            let mut a = Vec::new();
            for (a1, a2) in a1.iter().zip(a2) {
                a.push(unify(type_vars, a1, a2)?);
            }

            Ok(Type::App(Box::new(f), a))
        }

        _ => Err(TypeError {
            message: format!("types {t1} and {t2} cannot be unified"),
        }),
    }
}
