use std::collections::{HashMap, hash_map::Entry};

use super::ast_lowering::{SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {}

enum TypeConstraint<'a> {
    Int(Type<'a>),
    Float(Type<'a>),
    Equals(Type<'a>, Type<'a>),
    Nillable(Type<'a>),
}

#[allow(unused)]
fn create_loop_constraints<'a>(
    type_: &Type<'a>,
    sexpr: &mut SExpr<'a>,
    constraints: &mut Vec<TypeConstraint<'a>>,
) {
    match sexpr {
        SExpr::List { values, .. } => {
            for value in values {
                create_loop_constraints(type_, value, constraints);
            }
        }

        SExpr::Quote { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Comma { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Backtick { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Splice { value, .. } => create_loop_constraints(type_, value, constraints),

        SExpr::Seq { values, .. } => {
            for value in values {
                create_loop_constraints(type_, value, constraints);
            }
        }

        SExpr::Cond { values, .. } => {
            for (cond, then) in values {
                create_loop_constraints(type_, cond, constraints);
                create_loop_constraints(type_, then, constraints);
            }
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                create_loop_constraints(type_, &mut **value, constraints);
                constraints.push(TypeConstraint::Equals(
                    value.meta().type_.clone(),
                    type_.clone(),
                ));
            } else {
                constraints.push(TypeConstraint::Equals(type_.clone(), Type::Tuple(vec![])));
            }
        }

        SExpr::Type { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::FuncCall { func, values, .. } => todo!(),
        SExpr::StructSet { values, .. } => todo!(),
        SExpr::Declare { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Assign { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Attribute { top, attrs, .. } => todo!(),

        _ => (),
    }
}

fn create_constraints<'a>(
    sexpr: &mut SExpr<'a>,
    type_var_counter: &mut u64,
    constraints: &mut Vec<TypeConstraint<'a>>,
) {
    let meta = sexpr.meta_mut();
    if meta.type_ == Type::Unknown {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        *type_var_counter += 1;
    } else if meta.type_ == Type::UnknownInt {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        constraints.push(TypeConstraint::Int(meta.type_.clone()));
        *type_var_counter += 1;
    } else if meta.type_ == Type::UnknownFloat {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        constraints.push(TypeConstraint::Float(meta.type_.clone()));
        *type_var_counter += 1;
    }

    #[allow(unused)]
    match sexpr {
        SExpr::Int { .. } => (),
        SExpr::Float { .. } => (),
        SExpr::Str { .. } => (),
        SExpr::Symbol { meta, value } => (),

        SExpr::List { meta, values } => todo!(),
        SExpr::Quote { meta, value } => todo!(),
        SExpr::Comma { meta, value } => todo!(),
        SExpr::Backtick { meta, value } => todo!(),
        SExpr::Splice { meta, value } => todo!(),

        SExpr::Seq { meta, values } => {
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints);
            }

            constraints.push(TypeConstraint::Equals(
                meta.type_.clone(),
                values.last().unwrap().meta().type_.clone(),
            ));
        }

        SExpr::Cond { meta, values } => {
            for (cond, then) in values {
                create_constraints(cond, type_var_counter, constraints);
                constraints.push(TypeConstraint::Int(cond.meta().type_.clone()));
                create_constraints(then, type_var_counter, constraints);
                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    then.meta().type_.clone(),
                ));
            }
        }

        SExpr::Loop { meta, value } => {
            create_constraints(value, type_var_counter, constraints);
            create_loop_constraints(&meta.type_, value, constraints);
            constraints.push(TypeConstraint::Nillable(meta.type_.clone()));
        }

        SExpr::Break { meta, value } => {
            if let Some(value) = value {
                create_constraints(&mut **value, type_var_counter, constraints);
            }
        }

        SExpr::Nil { meta } => (),

        SExpr::Type { meta, value, type_ } => todo!(),
        SExpr::FuncDef {
            meta,
            name,
            ret_type,
            args,
            expr,
        } => todo!(),
        SExpr::FuncCall { meta, func, values } => todo!(),
        SExpr::StructDef { meta, name, fields } => todo!(),
        SExpr::StructSet {
            meta,
            struct_name,
            values,
        } => todo!(),
        SExpr::Declare {
            meta,
            mutable,
            variable,
            value,
        } => todo!(),
        SExpr::Assign {
            meta,
            variable,
            value,
        } => todo!(),
        SExpr::Attribute { meta, top, attrs } => todo!(),
    }
}

fn occurs_in(substitutions: &[Type<'_>], index: u64, t: &Type<'_>) -> bool {
    match t {
        Type::TypeVariable(i) if substitutions[*i as usize] != Type::TypeVariable(*i) => {
            occurs_in(substitutions, index, &substitutions[*i as usize])
        }
        Type::TypeVariable(i) => *i == index,

        Type::Tuple(v) => v.iter().any(|v| occurs_in(substitutions, index, v)),
        Type::Pointer(_, t) => occurs_in(substitutions, index, t),
        Type::Slice(_, t) => occurs_in(substitutions, index, t),
        Type::Struct(_, v) => v.iter().any(|v| occurs_in(substitutions, index, v)),
        Type::Function(v, t) => {
            v.iter().any(|v| occurs_in(substitutions, index, v))
                || occurs_in(substitutions, index, t)
        }
        _ => false,
    }
}

fn unify<'a>(substitutions: &mut Vec<Type<'a>>, t1: Type<'a>, t2: Type<'a>) {
    match (t1, t2) {
        (Type::TypeVariable(i), Type::TypeVariable(j)) if i == j => (),

        (Type::TypeVariable(i), t2) if Type::TypeVariable(i) != substitutions[i as usize] => {
            unify(substitutions, substitutions[i as usize].clone(), t2)
        }
        (t1, Type::TypeVariable(i)) if Type::TypeVariable(i) != substitutions[i as usize] => {
            unify(substitutions, t1, substitutions[i as usize].clone())
        }

        (Type::TypeVariable(i), t2) => {
            assert!(!occurs_in(substitutions, i, &t2));
            substitutions[i as usize] = t2;
        }

        (t1, Type::TypeVariable(i)) => {
            assert!(!occurs_in(substitutions, i, &t1));
            substitutions[i as usize] = t1;
        }
        (Type::Int(signed1, width1), Type::Int(signed2, width2))
            if signed1 == signed2 && width1 == width2 => (),

        (Type::F32, Type::F32) => (),

        (Type::F64, Type::F64) => (),

        (Type::Tuple(v1), Type::Tuple(v2)) => {
            if v1.len() != v2.len() {
                todo!("error handling");
            }

            for (t1, t2) in v1.into_iter().zip(v2) {
                unify(substitutions, t1, t2);
            }
        }

        (Type::Pointer(_, t1), Type::Pointer(_, t2)) => unify(substitutions, *t1, *t2),
        (Type::Slice(_, t1), Type::Slice(_, t2)) => unify(substitutions, *t1, *t2),

        (Type::Struct(name1, v1), Type::Struct(name2, v2)) if name1 == name2 => {
            if v1.len() != v2.len() {
                todo!("error handling");
            }

            for (t1, t2) in v1.into_iter().zip(v2) {
                unify(substitutions, t1, t2);
            }
        }

        (Type::Generic(_), Type::Generic(_)) => (),

        (Type::Function(v1, t1), Type::Function(v2, t2)) => {
            if v1.len() != v2.len() {
                todo!("error handling");
            }

            for (t1, t2) in v1.into_iter().zip(v2) {
                unify(substitutions, t1, t2);
            }

            unify(substitutions, *t1, *t2);
        }

        _ => todo!("error handling"),
    }
}

#[derive(Copy, Clone, PartialEq)]
enum FloatOrInt {
    Float,
    Int,
    Nil,
    Neither,
}

fn unify_types(type_var_counter: u64, constraints: Vec<TypeConstraint<'_>>) -> Vec<Type<'_>> {
    let mut substitutions = vec![Type::Unknown; type_var_counter as usize];
    let mut float_or_int = vec![FloatOrInt::Neither; type_var_counter as usize];

    for (i, sub) in substitutions.iter_mut().enumerate() {
        *sub = Type::TypeVariable(i as u64);
    }

    for constraint in constraints {
        match constraint {
            TypeConstraint::Int(t) => match t {
                Type::TypeVariable(i)
                    if matches!(
                        float_or_int[i as usize],
                        FloatOrInt::Int | FloatOrInt::Neither
                    ) =>
                {
                    float_or_int[i as usize] = FloatOrInt::Int
                }
                Type::Int(_, _) => (),
                _ => todo!("error handling"),
            },

            TypeConstraint::Float(t) => match t {
                Type::TypeVariable(i)
                    if matches!(
                        float_or_int[i as usize],
                        FloatOrInt::Float | FloatOrInt::Neither
                    ) =>
                {
                    float_or_int[i as usize] = FloatOrInt::Float
                }
                Type::F32 | Type::F64 => (),
                _ => todo!("error handling"),
            },

            TypeConstraint::Equals(t1, t2) => unify(&mut substitutions, t1, t2),

            TypeConstraint::Nillable(t) => match t {
                Type::TypeVariable(i)
                    if matches!(
                        float_or_int[i as usize],
                        FloatOrInt::Nil | FloatOrInt::Neither
                    ) =>
                {
                    float_or_int[i as usize] = FloatOrInt::Nil
                }
                _ => (),
            },
        }
    }

    for i in 0..substitutions.len() {
        let mut j = i;
        while let Type::TypeVariable(k) = substitutions[j] {
            if j == k as usize {
                break;
            }

            j = k as usize;
        }

        if j != i {
            substitutions[i] = substitutions[j].clone();

            match (float_or_int[i], float_or_int[j]) {
                (FloatOrInt::Neither, _) => float_or_int[i] = float_or_int[j],
                (_, FloatOrInt::Neither) => float_or_int[j] = float_or_int[i],
                (FloatOrInt::Nil, foi) if !matches!(foi, FloatOrInt::Neither) => {
                    float_or_int[i] = float_or_int[j]
                }
                (foi, FloatOrInt::Nil) if !matches!(foi, FloatOrInt::Neither) => {
                    float_or_int[j] = float_or_int[i]
                }
                (a, b) if a == b => (),
                _ => todo!("error handling"),
            }

            if matches!(float_or_int[j], FloatOrInt::Nil)
                && !matches!(float_or_int[i], FloatOrInt::Nil | FloatOrInt::Neither)
            {
                float_or_int[j] = float_or_int[i];
            } else {
                float_or_int[i] = float_or_int[j];
            }
        }
    }

    for (i, foi) in float_or_int.iter_mut().enumerate() {
        match foi {
            FloatOrInt::Float => match &substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::F64;
                }

                Type::F32 | Type::F64 => (),

                _ => todo!("error handling"),
            },

            FloatOrInt::Int => match &substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::Int(true, 32);
                }

                Type::Int(_, _) => (),

                _ => todo!("error handling"),
            },

            FloatOrInt::Neither => (),

            FloatOrInt::Nil => {
                if let Type::TypeVariable(_) = substitutions[i] {
                    substitutions[i] = Type::Tuple(vec![])
                }
            }
        }
    }

    for sub in substitutions.iter() {
        if let Type::TypeVariable(_) = sub {
            todo!("error handling");
        }
    }

    substitutions
}

pub fn check(sexprs: &mut [SExpr<'_>]) -> Result<(), CorrectnessError> {
    let mut type_var_counter = 0;
    let mut constraints = vec![];

    for sexpr in sexprs {
        create_constraints(sexpr, &mut type_var_counter, &mut constraints);
    }

    let unified = unify_types(type_var_counter, constraints);
    println!("{:?}", unified);

    Ok(())
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub arg_types: Vec<Type<'a>>,
    pub ret_type: Type<'a>,
    pub index: Option<usize>,
}

pub fn extract_signatures<'a>(sexprs: &[SExpr<'a>], map: &mut HashMap<&'a str, Vec<Signature<'a>>>) {
    for (i, sexpr) in sexprs.iter().enumerate() {
        if let SExpr::FuncDef { meta, name, .. } = sexpr {
            let (arg_types, ret_type) = match &meta.type_ {
                Type::Function(a, r) => (a.clone(), (**r).clone()),
                _ => unreachable!(),
            };
            let index = Some(i);

            match map.entry(*name) {
                Entry::Occupied(mut entry) => {
                    for sig in entry.get() {
                        if sig.arg_types == arg_types && sig.ret_type == ret_type {
                            todo!("error handling");
                        }
                    }

                    entry.get_mut().push(Signature {
                        arg_types,
                        ret_type,
                        index,
                    });
                }

                Entry::Vacant(entry) => {
                    entry.insert(vec![Signature {
                        arg_types,
                        ret_type,
                        index
                    }]);
                }
            }
        }
    }
}
