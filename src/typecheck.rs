use std::collections::{HashMap, hash_map::Entry};

use crate::ast::*;

struct TypeData {
    va_params: bool,
    params: Vec<String>,
    variants: Vec<Variant>,
}

pub struct TypeError {
    pub message: String,
}

pub fn typecheck(ast: &[TopLevel]) -> Result<(), TypeError> {
    // set up scopes
    let mut globals = HashMap::new();
    let mut defined_types = HashMap::new();
    defined_types.insert("Fn".to_owned(), TypeData {
        va_params: false,
        params: vec!["A".to_owned(), "R".to_owned()],
        variants: Vec::new(),
    });
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

                    v.insert(Type::App(
                        Box::new(Type::Name("Fn".to_owned())),
                        vec![
                            Type::App(Box::new(Type::Name("Tuple".to_owned())), variant.fields.clone()),
                            self_type.clone()]));
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
                v.insert(Type::App(
                    Box::new(Type::Name("Fn".to_owned())),
                    vec![Type::App(Box::new(Type::Name("Tuple".to_owned())), fn_args), ret.clone()]));
            }
        }
    }

    // verify types in globals (type variants are globals so this is sufficient)
    for (_, t) in globals.iter() {
        verify_type(t, &defined_types)?;
    }

    // typecheck statements
    for top in ast {
        let TopLevel::FuncDef { args, ret, stats, .. } = top
        else {
            continue;
        };

        let mut scopes = vec![HashMap::new()];
        for (name, ty) in args {
            scopes[0].insert(name.clone(), ty.clone());
        }

        for stat in stats {
            typecheck_stat(&globals, &mut scopes, stat, &ret)?;
        }
    }

    Ok(())
}

fn typecheck_stat(
    globals: &HashMap<String, Type>,
    scopes: &mut Vec<HashMap<String, Type>>,
    stat: &Statement,
    expected_ret: &Type
) -> Result<(), TypeError> {
    match stat {
        Statement::FuncCall { func, args } => todo!(),
        Statement::Let { name, value } => todo!(),
        Statement::Set { name, value } => todo!(),
        Statement::Loop { body } => todo!(),
        Statement::Break => todo!(),
        Statement::Continue => todo!(),

        Statement::Return(v) => {
            let ty = match v {
                Some(v) => typecheck_expr(globals, scopes, &v)?,
                None => Type::Name("Unit".to_owned()),
            };
            unify(&ty, expected_ret)?;
            Ok(())
        }

        Statement::If { cond, then, elsy } => todo!(),
        Statement::Match { value, branches } => todo!(),
    }
}

fn typecheck_expr(
    globals: &HashMap<String, Type>,
    scopes: &mut Vec<HashMap<String, Type>>,
    expr: &Expr,
) -> Result<Type, TypeError> {
    match expr {
        Expr::Integer(_) => Ok(Type::Name("Int".to_string())),
        Expr::Symbol(_) => todo!(),
        Expr::String(_) => Ok(Type::Name("String".to_string())),
        Expr::FuncCall { func, args } => todo!(),
    }
}

fn verify_type(t: &Type, defined_types: &HashMap<String, TypeData>) -> Result<(), TypeError> {
    fn helper(t: &Type, defined_types: &HashMap<String, TypeData>) -> Result<Option<usize>, TypeError> {
        match t {
            Type::Typevar(_) | Type::Generic(_) => Ok(None),

            Type::Name(n) => {
                let Some(data) = defined_types.get(n)
                else {
                    return Err(TypeError {
                        message: format!("type {} was never defined", t),
                    });
                };

                if data.va_params {
                    Ok(None)
                } else {
                    Ok(Some(data.params.len()))
                }
            }

            Type::App(f, a) => {
                for a in a {
                    if let Some(0) | None = helper(a, defined_types)? { }
                    else {
                        return Err(TypeError {
                            message: format!("type parameter {a} should have 0 expected type parameters"),
                        });
                    }
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

fn unify(
    t1: &Type,
    t2: &Type,
) -> Result<Type, TypeError> {
    match (t1, t2) {
        (Type::Name(n1), Type::Name(n2)) if n1 == n2 => Ok(t1.clone()),
        (Type::App(f1, a1), Type::App(f2, a2)) => {
            let f = unify(f1, f2)?;

            if a1.len() != a2.len() {
                return Err(TypeError {
                    message: format!("types {} and {} cannot be unified", t1, t2),
                });
            }

            let mut a = Vec::new();
            for (a1, a2) in a1.iter().zip(a2) {
                a.push(unify(a1, a2)?);
            }

            Ok(Type::App(Box::new(f), a))
        }

        _ => Err(TypeError {
            message: format!("types {t1} and {t2} cannot be unified"),
        }),
    }
}
