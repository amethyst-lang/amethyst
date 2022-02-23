use super::ast_lowering::{SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {
}

enum TypeConstraint<'a> {
    Int(Type<'a>),
    Float(Type<'a>),
    Equals(Type<'a>, Type<'a>),
}

fn create_constraints<'a>(sexpr: &mut SExpr<'a>, type_var_counter: &mut u64, constraints: &mut Vec<TypeConstraint<'a>>) {
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
        SExpr::Int { meta, value } => (),
        SExpr::Float { meta, value } => (),
        SExpr::Str { meta, value } => (),

        SExpr::Symbol { meta, value } => todo!(),
        SExpr::List { meta, values } => todo!(),
        SExpr::Quote { meta, value } => todo!(),
        SExpr::Comma { meta, value } => todo!(),
        SExpr::Backtick { meta, value } => todo!(),
        SExpr::Splice { meta, value } => todo!(),

        SExpr::Seq { meta, values } => {
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints);
            }

            constraints.push(TypeConstraint::Equals(meta.type_.clone(), values.last().unwrap().meta().type_.clone()));
        }

        SExpr::Cond { meta, values } => todo!(),
        SExpr::Loop { meta, value } => todo!(),
        SExpr::Break { meta, value } => todo!(),

        SExpr::Nil { meta } => (),

        SExpr::Type { meta, value, type_ } => todo!(),
        SExpr::FuncDef { meta, name, ret_type, args } => todo!(),
        SExpr::FuncCall { meta, func, values } => todo!(),
        SExpr::StructDef { meta, name, fields } => todo!(),
        SExpr::StructSet { meta, struct_name, values } => todo!(),
        SExpr::Declare { meta, mutable, variable, value } => todo!(),
        SExpr::Assign { meta, variable, value } => todo!(),
        SExpr::Attribute { meta, top, attrs } => todo!(),
    }
}

fn occurs_in(substitutions: &[Type<'_>], index: u64, t: &Type<'_>) -> bool {
    match t {
        Type::TypeVariable(i) if substitutions[*i as usize] != Type::TypeVariable(*i) => occurs_in(substitutions, index, &substitutions[*i as usize]),
        Type::TypeVariable(i) => *i == index,

        Type::Tuple(v) => v.iter().any(|v| occurs_in(substitutions, index, v)),
        Type::Pointer(_, t) => occurs_in(substitutions, index, t),
        Type::FatPointer(_, t) => occurs_in(substitutions, index, t),
        Type::Struct(_, v) => v.iter().any(|v| occurs_in(substitutions, index, v)),
        Type::Function(v, t) => v.iter().any(|v| occurs_in(substitutions, index, v)) || occurs_in(substitutions, index, t),
        _ => false,
    }
}

fn unify<'a>(substitutions: &mut Vec<Type<'a>>, t1: Type<'a>, t2: Type<'a>) {
    match (t1, t2) {
        (Type::TypeVariable(i), Type::TypeVariable(j)) if i == j => (),

        (Type::TypeVariable(i), t2) if Type::TypeVariable(i) != substitutions[i as usize] => unify(substitutions, substitutions[i as usize].clone(), t2),
        (t1, Type::TypeVariable(i)) if Type::TypeVariable(i) != substitutions[i as usize] => unify(substitutions, t1, substitutions[i as usize].clone()),

        (Type::TypeVariable(i), t2) => {
            assert!(!occurs_in(substitutions, i, &t2));
            substitutions[i as usize] = t2;
        }

        (t1, Type::TypeVariable(i)) => {
            assert!(!occurs_in(substitutions, i, &t1));
            substitutions[i as usize] = t1;
        }
        (Type::Int(signed1, width1), Type::Int(signed2, width2)) if signed1 == signed2 && width1 == width2 => (),

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
        (Type::FatPointer(_, t1), Type::FatPointer(_, t2)) => unify(substitutions, *t1, *t2),

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

#[derive(Copy, Clone)]
enum FloatOrInt {
    Float,
    Int,
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
            TypeConstraint::Int(t) => {
                match t {
                    Type::TypeVariable(i) => float_or_int[i as usize] = FloatOrInt::Int,
                    Type::Int(_, _) => (),
                    _ => todo!("error handling"),
                }
            }

            TypeConstraint::Float(t) => {
                match t {
                    Type::TypeVariable(i) => float_or_int[i as usize] = FloatOrInt::Float,
                    Type::Int(_, _) => (),
                    _ => todo!("error handling"),
                }
            }

            TypeConstraint::Equals(t1, t2) => unify(&mut substitutions, t1, t2),
        }
    }

    for (mut i, foi) in float_or_int.iter_mut().enumerate() {
        match foi {
            FloatOrInt::Float => {
                while let Type::TypeVariable(j) = substitutions[i] {
                    if i == j as usize {
                        break;
                    }

                    i = j as usize;
                }

                match &substitutions[i] {
                    Type::TypeVariable(_) => {
                        substitutions[i] = Type::F64;
                    }

                    Type::F32 | Type::F64 => (),

                    _ => todo!("error handling"),
                }
            }

            FloatOrInt::Int => {
                while let Type::TypeVariable(j) = substitutions[i] {
                    if i == j as usize {
                        break;
                    }

                    i = j as usize;
                }

                match &substitutions[i] {
                    Type::TypeVariable(_) => {
                        substitutions[i] = Type::Int(true, 32);
                    }

                    Type::Int(_, _) => (),

                    _ => todo!("error handling"),
                }
            }

            FloatOrInt::Neither => (),
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
