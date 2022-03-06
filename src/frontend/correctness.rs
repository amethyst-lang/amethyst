use std::collections::{HashMap, hash_map::Entry};

use super::ast_lowering::{SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {
    UnificationError,
}

enum TypeConstraint<'a> {
    Int(Type<'a>),
    Float(Type<'a>),
    Equals(Type<'a>, Type<'a>),
    Options(Type<'a>, Vec<Type<'a>>),
    Nillable(Type<'a>),
    Attribute(Type<'a>, Type<'a>, Vec<&'a str>),
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

        SExpr::Cond { values, elsy, .. } => {
            for (cond, then) in values {
                create_loop_constraints(type_, cond, constraints);
                create_loop_constraints(type_, then, constraints);
            }

            if let Some(elsy) = elsy {
                create_loop_constraints(type_, &mut **elsy, constraints);
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

        SExpr::FuncCall { func, values, .. } => {
            create_loop_constraints(type_, &mut **func, constraints);
            for value in values.iter_mut() {
                create_loop_constraints(type_, value, constraints);
            }
        }

        SExpr::StructSet { values, .. } => todo!(),
        SExpr::Declare { value, .. } => create_loop_constraints(type_, value, constraints),
        SExpr::Assign { value, .. } => create_loop_constraints(type_, value, constraints),

        SExpr::Attribute { top, attrs, .. } => {
            create_loop_constraints(type_, top, constraints);
        }

        _ => (),
    }
}

fn create_constraints<'a>(
    sexpr: &mut SExpr<'a>,
    type_var_counter: &mut u64,
    constraints: &mut Vec<TypeConstraint<'a>>,
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
    monomorphisms: &mut Vec<(Type<'a>, usize)>,
    scopes: &mut Vec<HashMap<&'a str, Type<'a>>>,
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

        SExpr::Symbol { meta, value } => {
            for scope in scopes.iter().rev() {
                if let Some(t) = scope.get(value) {
                    constraints.push(TypeConstraint::Equals(meta.type_.clone(), t.clone()));
                    return;
                }
            }

            if let Some(v) = func_map.get(value) {
                if v.len() == 1 && v[0].arg_types.iter().all(|v| !v.has_generic()) && !v[0].ret_type.has_generic() {
                    constraints.push(TypeConstraint::Equals(meta.type_.clone(), Type::Function(v[0].arg_types.clone(), Box::new(v[0].ret_type.clone()))));
                }
            }
        }

        SExpr::List { meta, values } => todo!(),
        SExpr::Quote { meta, value } => todo!(),
        SExpr::Comma { meta, value } => todo!(),
        SExpr::Backtick { meta, value } => todo!(),
        SExpr::Splice { meta, value } => todo!(),

        SExpr::Seq { meta, values } => {
            scopes.push(HashMap::new());
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            }
            scopes.pop();

            constraints.push(TypeConstraint::Equals(
                meta.type_.clone(),
                values.last().unwrap().meta().type_.clone(),
            ));
        }

        SExpr::Cond { meta, values, elsy } => {
            for (cond, then) in values {
                scopes.push(HashMap::new());
                create_constraints(cond, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
                constraints.push(TypeConstraint::Int(cond.meta().type_.clone()));
                create_constraints(then, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
                scopes.pop();

                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    then.meta().type_.clone(),
                ));
            }

            if let Some(elsy) = elsy {
                create_constraints(elsy, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);

                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    elsy.meta().type_.clone(),
                ));
            }
        }

        SExpr::Loop { meta, value } => {
            scopes.push(HashMap::new());
            create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            create_loop_constraints(&meta.type_, value, constraints);
            constraints.push(TypeConstraint::Nillable(meta.type_.clone()));
            scopes.pop();
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            }
        }

        SExpr::Nil { .. } => (),

        SExpr::Type { meta, value } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
        }

        SExpr::FuncDef {
            args,
            ret_type,
            expr,
            ..
        } => {
            if args.iter().any(|(_, v)| v.has_generic()) || ret_type.has_generic() {
                return;
            }

            let mut scope = HashMap::new();
            for (name, type_) in args.iter() {
                scope.insert(*name, type_.clone());
            }

            scopes.push(scope);
            create_constraints(&mut **expr, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            constraints.push(TypeConstraint::Equals(ret_type.clone(), expr.meta().type_.clone()));
            scopes.pop();
        }

        SExpr::FuncCall { meta, func, values } => {
            create_constraints(&mut **func, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            let mut args = vec![];
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
                args.push(value.meta().type_.clone());
            }
            let func_type_var = Type::Function(args, Box::new(meta.type_.clone()));

            constraints.push(TypeConstraint::Equals(func.meta().type_.clone(), func_type_var.clone()));

            if let SExpr::Symbol { value, .. } = **func {
                if let Some(signatures) = func_map.get(value) {
                    let mut valid_sigs: Vec<_> = signatures.iter().filter_map(|v| {
                        if v.arg_types.len() == values.len() {
                            let mut sig = Type::Function(v.arg_types.clone(), Box::new(v.ret_type.clone()));
                            if sig.has_generic() {
                                let mut map = HashMap::new();
                                sig.replace_generics(type_var_counter, &mut map);
                            }
                            Some(sig)
                        } else {
                            None
                        }
                    }).collect();

                    if !valid_sigs.is_empty() {
                        if valid_sigs.len() == 1 {
                            constraints.push(TypeConstraint::Equals(func_type_var, valid_sigs.remove(0)));
                        } else {
                            constraints.push(TypeConstraint::Options(func_type_var, valid_sigs));
                        }
                    }
                }
            }
        }

        SExpr::StructDef { meta, name, fields } => (),

        SExpr::StructSet {
            meta,
            name,
            values,
        } => {
            if let Some(struct_) = struct_map.get(name) {
                let mut map = HashMap::new();
                let mut generics = vec![];

                for (field, value) in values {
                    create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);

                    if let Some((_, t)) = struct_.fields.iter().find(|(name, _)| *name == *field) {
                        let mut t = t.clone();
                        t.find_generics(&mut generics);
                        t.replace_generics(type_var_counter, &mut map);
                        constraints.push(TypeConstraint::Equals(t, value.meta().type_.clone()));
                    }
                }

                constraints.push(TypeConstraint::Equals(meta.type_.clone(), Type::Struct(struct_.name, generics.into_iter().map(|v| {
                    match v {
                        Type::Generic(g) => map.remove(g).unwrap(),
                        _ => unreachable!(),
                    }
                }).collect())));
            }
        }

        SExpr::Declare {
            meta,
            mutable: _,
            variable,
            value,
        } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            if let Some(scope) = scopes.last_mut() {
                scope.insert(*variable, value.meta().type_.clone());
            }
            constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
        }

        SExpr::Assign {
            meta,
            variable,
            value,
        } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            for scope in scopes.iter().rev() {
                if let Some(t) = scope.get(variable) {
                    constraints.push(TypeConstraint::Equals(value.meta().type_.clone(), t.clone()));
                    constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
                }
            }
        }

        SExpr::Attribute { meta, top, attrs } => {
            create_constraints(&mut **top, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes);
            constraints.push(TypeConstraint::Attribute(meta.type_.clone(), top.meta().type_.clone(), attrs.clone()));
        }
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

fn unify<'a>(substitutions: &mut Vec<Type<'a>>, t1: &Type<'a>, t2: &Type<'a>) -> Result<(), CorrectnessError> {
    match (t1, t2) {
        (Type::TypeVariable(i), Type::TypeVariable(j)) if i == j => Ok(()),

        (Type::TypeVariable(i), t2) if Type::TypeVariable(*i) != substitutions[*i as usize] => {
            unify(substitutions, &substitutions[*i as usize].clone(), t2)
        }
        (t1, Type::TypeVariable(i)) if Type::TypeVariable(*i) != substitutions[*i as usize] => {
            unify(substitutions, t1, &substitutions[*i as usize].clone())
        }

        (Type::TypeVariable(i), t2) => {
            if occurs_in(substitutions, *i, t2) {
                return Err(CorrectnessError::UnificationError);
            }

            substitutions[*i as usize] = t2.clone();
            Ok(())
        }

        (t1, Type::TypeVariable(i)) => {
            if occurs_in(substitutions, *i, t1) {
                return Err(CorrectnessError::UnificationError);
            }

            substitutions[*i as usize] = t1.clone();
            Ok(())
        }

        (Type::Int(signed1, width1), Type::Int(signed2, width2))
            if signed1 == signed2 && width1 == width2 => Ok(()),

        (Type::F32, Type::F32) => Ok(()),

        (Type::F64, Type::F64) => Ok(()),

        (Type::Tuple(v1), Type::Tuple(v2)) => {
            if v1.len() != v2.len() {
                return Err(CorrectnessError::UnificationError);
            }

            for (t1, t2) in v1.iter().zip(v2) {
                unify(substitutions, t1, t2)?;
            }

            Ok(())
        }

        (Type::Pointer(_, t1), Type::Pointer(_, t2)) => unify(substitutions, &**t1, &**t2),
        (Type::Slice(_, t1), Type::Slice(_, t2)) => unify(substitutions, &**t1, &**t2),

        (Type::Struct(name1, v1), Type::Struct(name2, v2)) if name1 == name2 => {
            if v1.len() != v2.len() {
                return Err(CorrectnessError::UnificationError);
            }

            for (t1, t2) in v1.iter().zip(v2) {
                unify(substitutions, t1, t2)?;
            }

            Ok(())
        }

        (Type::Generic(_), Type::Generic(_)) => Ok(()),

        (Type::Function(v1, t1), Type::Function(v2, t2)) => {
            if v1.len() != v2.len() {
                return Err(CorrectnessError::UnificationError);
            }

            for (t1, t2) in v1.iter().zip(v2) {
                unify(substitutions, t1, t2)?;
            }

            unify(substitutions, &**t1, &**t2)
        }

        _ => Err(CorrectnessError::UnificationError),
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum FloatOrInt {
    Float,
    Int,
    Nil,
    Neither,
}

fn propagator_helper<'a>(t: &mut Type<'a>, substitutions: &[Type<'a>]) {
    match t {
        Type::Tuple(v) => {
            for v in v {
                propagator_helper(v, substitutions);
            }
        }

        Type::Pointer(_, v) => propagator_helper(&mut **v, substitutions),
        Type::Slice(_, v) => propagator_helper(&mut **v, substitutions),

        Type::Struct(_, v) => {
            for v in v {
                propagator_helper(v, substitutions);
            }
        }

        Type::Function(a, r) => {
            for a in a {
                propagator_helper(a, substitutions);
            }

            propagator_helper(r, substitutions);
        }

        Type::TypeVariable(i) if !matches!(&substitutions[*i as usize], Type::Unknown) && !matches!(&substitutions[*i as usize], Type::TypeVariable(j) if *i == *j) => {
            *t = substitutions[*i as usize].clone();
            propagator_helper(t, substitutions);
        }

        _ => (),
    }
}

fn unify_type_variable_propagator(substitutions: &mut Vec<Type<'_>>, float_or_int: &mut Vec<FloatOrInt>) -> Result<(), CorrectnessError> {
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
                _ => return Err(CorrectnessError::UnificationError),
            }

            if matches!(float_or_int[j], FloatOrInt::Nil)
                && !matches!(float_or_int[i], FloatOrInt::Nil | FloatOrInt::Neither)
            {
                float_or_int[j] = float_or_int[i];
            } else {
                float_or_int[i] = float_or_int[j];
            }
        }

        let mut temp = Type::Unknown;
        std::mem::swap(&mut temp, &mut substitutions[i]);
        propagator_helper(&mut temp, substitutions);
        std::mem::swap(&mut temp, &mut substitutions[i]);
    }

    Ok(())
}

fn check_float_or_int(substitutions: &mut Vec<Type<'_>>, float_or_int: &[FloatOrInt]) -> Result<(), CorrectnessError> {
    for (i, foi) in float_or_int.iter().enumerate() {
        match foi {
            FloatOrInt::Float => match substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::F64;
                }

                Type::F32 | Type::F64 => (),

                _ => return Err(CorrectnessError::UnificationError),
            },

            FloatOrInt::Int => match substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::Int(true, 32);
                }

                Type::Int(_, _) => (),

                _ => return Err(CorrectnessError::UnificationError),
            },

            FloatOrInt::Neither => (),

            FloatOrInt::Nil => {
                if let Type::TypeVariable(_) = substitutions[i] {
                    substitutions[i] = Type::Tuple(vec![])
                }
            }
        }
    }

    Ok(())
}

fn unify_attrs<'a>(attrs: &[(Type<'a>, Type<'a>, Vec<&'a str>)], substitutions: &mut Vec<Type<'a>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<(), CorrectnessError> {
    for (result, obj, attrs) in attrs {
        let mut obj = obj.clone();
        while let Type::TypeVariable(i) = obj {
            let new = substitutions[i as usize].clone();
            match new {
                Type::TypeVariable(j) if i == j => return Err(CorrectnessError::UnificationError),
                _ => obj = new,
            }
        }

        for attr in attrs {
            match obj {
                Type::Struct(name, generics) => {
                    if let Some(struct_) = struct_map.get(&name) {
                        if let Some((_, t)) = struct_.fields.iter().find(|(v, _)| *v == *attr) {
                            let mut type_var_counter = 0;
                            let mut map = struct_.generics.iter().map(|v| {
                                if let Type::Generic(g) = *v {
                                    g
                                } else {
                                    unreachable!("must be all generics");
                                }
                            }).zip(generics.into_iter()).collect();
                            obj = t.clone();
                            obj.replace_generics(&mut type_var_counter, &mut map);
                        } else {
                            return Err(CorrectnessError::UnificationError);
                        }
                    } else {
                        return Err(CorrectnessError::UnificationError);
                    }
                }

                Type::Slice(mutable, t) => {
                    match *attr {
                        "len" | "cap" => obj = Type::Int(false, 64),
                        "ptr" => obj = Type::Pointer(mutable, t),
                        _ => return Err(CorrectnessError::UnificationError),
                    }
                }

                Type::TypeVariable(_) => (),

                _ => return Err(CorrectnessError::UnificationError)
            }
        }

        match (result, obj) {
            (Type::TypeVariable(i), Type::TypeVariable(j)) if *i == j => return Err(CorrectnessError::UnificationError),

            (_, Type::TypeVariable(_)) => (),

            (Type::TypeVariable(i), t) => {
                match &substitutions[*i as usize] {
                    Type::TypeVariable(j) if *i == *j => substitutions[*i as usize] = t,
                    _ => unify(substitutions, result, &t)?,
                }
            }

            (a, b) if *a == b => (),

            _ => return Err(CorrectnessError::UnificationError),
        }
    }

    Ok(())
}

fn unify_types<'a>(type_var_counter: u64, constraints: Vec<TypeConstraint<'a>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<Vec<Type<'a>>, CorrectnessError> {
    let mut substitutions = vec![Type::Unknown; type_var_counter as usize];
    let mut float_or_int = vec![FloatOrInt::Neither; type_var_counter as usize];
    let mut unification_options = vec![];
    let mut attrs = vec![];

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
                _ => return Err(CorrectnessError::UnificationError)
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
                _ => return Err(CorrectnessError::UnificationError)
            },

            TypeConstraint::Equals(t1, t2) => unify(&mut substitutions, &t1, &t2)?,

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

            TypeConstraint::Options(t1, options) => {
                unification_options.push((t1, options));
            }

            TypeConstraint::Attribute(result, obj, attrs2) => {
                attrs.push((result, obj, attrs2));
            }
        }
    }

    if !unification_options.is_empty() {
        let mut selected = vec![0; unification_options.len()];
        let mut successes = vec![];

        'b: loop {
            let mut sub = substitutions.clone();
            let mut success = true;

            for (i, (t, options)) in unification_options.iter().enumerate() {
                if unify(&mut sub, t, &options[selected[i]]).is_err() {
                    success = false;
                    break;
                }
            }

            if success && unify_attrs(&attrs, &mut sub, struct_map).is_ok() && check_float_or_int(&mut sub, &float_or_int).is_ok() {
                successes.push(sub);
            }

            let last_index = selected.len() - 1;
            for (i, selection) in selected.iter_mut().enumerate() {
                *selection += 1;

                if unification_options[i].1.len() > *selection {
                    break;
                } else {
                    *selection = 0;
                    if i == last_index {
                        break 'b;
                    }
                }
            }
        }

        if successes.len() == 1 {
            substitutions = successes.remove(0);
            unify_type_variable_propagator(&mut substitutions, &mut float_or_int)?;
        } else {
            return Err(CorrectnessError::UnificationError);
        }
    } else {
        unify_type_variable_propagator(&mut substitutions, &mut float_or_int)?;
        check_float_or_int(&mut substitutions, &float_or_int)?;
        unify_type_variable_propagator(&mut substitutions, &mut float_or_int)?;
        unify_attrs(&attrs, &mut substitutions, struct_map)?;
    }

    Ok(substitutions)
}

fn flatten_substitution<'a>(t: &mut Type<'a>, substitutions: &[Type<'a>]) {
    while let Type::TypeVariable(i) = t {
        match &substitutions[*i as usize] {
            Type::TypeVariable(j) if *i == *j => todo!("error handling for {}", j),
            _ => *t = substitutions[*i as usize].clone(),
        }
    }

    match t {
        Type::Tuple(v) => {
            for v in v {
                flatten_substitution(v, substitutions);
            }
        }

        Type::Pointer(_, v) => flatten_substitution(v, substitutions),
        Type::Slice(_, v) => flatten_substitution(v, substitutions),
        Type::Struct(_, v) => {
            for v in v {
                flatten_substitution(v, substitutions);
            }
        }

        Type::Function(a, r) => {
            for a in a {
                flatten_substitution(a, substitutions);
            }
            flatten_substitution(r, substitutions);
        }

        _ => (),
    }
}

fn apply_substitutions<'a>(sexpr: &mut SExpr<'a>, substitutions: &[Type<'a>]) {
    flatten_substitution(&mut sexpr.meta_mut().type_, substitutions);

    match sexpr {
        //SExpr::List { meta, values } => todo!(),
        //SExpr::Quote { meta, value } => todo!(),
        //SExpr::Comma { meta, value } => todo!(),
        //SExpr::Backtick { meta, value } => todo!(),
        //SExpr::Splice { meta, value } => todo!(),
        SExpr::Seq { values, .. } => {
            for value in values {
                apply_substitutions(value, substitutions);
            }
        }

        SExpr::Cond { values, elsy, .. } => {
            for (cond, then) in values {
                apply_substitutions(cond, substitutions);
                apply_substitutions(then, substitutions);
            }

            if let Some(elsy) = elsy {
                apply_substitutions(&mut **elsy, substitutions);
            }
        }

        SExpr::Loop { value, .. }
            | SExpr::Type { value, .. }
            | SExpr::Declare { value, .. }
            | SExpr::Assign { value, .. }
            | SExpr::Break { value: Some(value), .. } => apply_substitutions(value, substitutions),

        SExpr::FuncDef { expr, .. } => apply_substitutions(expr, substitutions),

        SExpr::FuncCall { func, values, .. } => {
            apply_substitutions(func, substitutions);
            for value in values {
                apply_substitutions(value, substitutions);
            }
        }

        SExpr::StructSet { values, .. } => {
            for (_, value) in values {
                apply_substitutions(value, substitutions);
            }
        }
        SExpr::Attribute { top, .. } => apply_substitutions(top, substitutions),
        _ => (),
    }
}

pub fn check<'a>(sexprs: &mut Vec<SExpr<'a>>, func_map: &HashMap<&'a str, Vec<Signature<'a>>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<(), CorrectnessError> {
    let mut monomorphisms;
    let mut skip = 0;
    let mut scopes = vec![HashMap::new()];

    while {
        let mut type_var_counter = 0;
        let mut constraints = vec![];
        monomorphisms = vec![];

        for sexpr in sexprs.iter_mut().skip(skip) {
            create_constraints(sexpr, &mut type_var_counter, &mut constraints, func_map, struct_map, &mut monomorphisms, &mut scopes);
        }

        let unified = unify_types(type_var_counter, constraints, struct_map)?;
        for sexpr in sexprs.iter_mut().skip(skip) {
            apply_substitutions(sexpr, &unified);
        }

        skip = sexprs.len();

        if !monomorphisms.is_empty() {
            for (mut t, index) in monomorphisms {
                let mut monomorphised = sexprs[index].clone();

                if let SExpr::FuncDef { meta, args, ret_type, .. } = &mut monomorphised {
                    flatten_substitution(&mut t, &unified);
                    meta.type_ = t.clone();
                    if let Type::Function(a, r) = t {
                        for (i, (_, arg)) in args.iter_mut().enumerate() {
                            *arg = a[i].clone();
                        }

                        *ret_type = *r;

                        sexprs.push(monomorphised);
                    }
                }
            }

            true
        } else {
            false
        }
    } {}

    Ok(())
}

#[derive(Debug, Clone)]
pub struct Signature<'a> {
    pub arg_types: Vec<Type<'a>>,
    pub ret_type: Type<'a>,
    pub index: Option<usize>,
}

fn create_one_signature<'a>(map: &mut HashMap<&'a str, Vec<Signature<'a>>>, name: &'a str, include_floats: bool, rets_bool: bool) {
    let mut sigs = vec![
        Signature {
            arg_types: vec![Type::Int(false, 8), Type::Int(false, 8)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(false, 8)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(false, 16), Type::Int(false, 16)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(false, 16)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(false, 32), Type::Int(false, 32)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(false, 32)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(false, 64), Type::Int(false, 64)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(false, 64)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(true, 8), Type::Int(true, 8)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(true, 8)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(true, 16), Type::Int(true, 16)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(true, 16)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(true, 32), Type::Int(true, 32)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(true, 32)
            },
            index: None,
        },
        Signature {
            arg_types: vec![Type::Int(true, 64), Type::Int(true, 64)],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::Int(true, 64)
            },
            index: None,
        },
    ];

    if include_floats {
        sigs.push(Signature {
            arg_types: vec![Type::F32, Type::F32],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::F32
            },
            index: None,
        });
        sigs.push(Signature {
            arg_types: vec![Type::F64, Type::F64],
            ret_type: if rets_bool {
                Type::Int(false, 1)
            } else {
                Type::F64
            },
            index: None,
        });
    }

    map.insert(name, sigs);
}

pub fn create_default_signatures<'a>() -> HashMap<&'a str, Vec<Signature<'a>>> {
    let mut map = HashMap::new();
    create_one_signature(&mut map, "+", true, false);
    map.get_mut("+").unwrap().extend([
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(true, 32)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(true, 64)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 32)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 64)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    create_one_signature(&mut map, "-", true, false);
    map.get_mut("-").unwrap().extend([
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(true, 32)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(true, 64)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 32)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 64)],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    create_one_signature(&mut map, "*", true, false);
    create_one_signature(&mut map, "/", true, false);
    create_one_signature(&mut map, "%", false, false);
    create_one_signature(&mut map, "<", true, true);
    create_one_signature(&mut map, ">", true, true);
    create_one_signature(&mut map, "<=", true, true);
    create_one_signature(&mut map, ">=", true, true);
    create_one_signature(&mut map, "<<", false, false);
    create_one_signature(&mut map, ">>", false, false);
    create_one_signature(&mut map, "&", false, false);
    create_one_signature(&mut map, "|", false, false);
    create_one_signature(&mut map, "^", false, false);
    create_one_signature(&mut map, "==", true, true);
    map.get_mut("==").unwrap().push(Signature {
        arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    create_one_signature(&mut map, "!=", true, true);
    map.get_mut("!=").unwrap().push(Signature {
        arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert("and", vec![
        Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        },
    ]);
    map.insert("or", vec![
        Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        },
    ]);
    map.insert("xor", vec![
        Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        },
    ]);

    let mut sigs = vec![];
    for a in [Type::Int(false, 8), Type::Int(false, 16), Type::Int(false, 32), Type::Int(false, 64), Type::Int(true, 8), Type::Int(true, 16), Type::Int(true, 32), Type::Int(true, 64)] {
        for r in [Type::Int(false, 8), Type::Int(false, 16), Type::Int(false, 32), Type::Int(false, 64), Type::Int(true, 8), Type::Int(true, 16), Type::Int(true, 32), Type::Int(true, 64)] {
            if a != r {
                sigs.push(Signature {
                    arg_types: vec![a.clone()],
                    ret_type: r,
                    index: None,
                });
            }
        }
    }
    sigs.push(Signature {
        arg_types: vec![Type::Pointer(false, Box::new(Type::Generic("a")))],
        ret_type: Type::Int(false, 64),
        index: None,
    });
    sigs.push(Signature {
        arg_types: vec![Type::Int(false, 64)],
        ret_type: Type::Pointer(false, Box::new(Type::Generic("a"))),
        index: None,
    });
    map.insert("cast", sigs);
    map.insert("alloca", vec![
        Signature {
            arg_types: vec![Type::Int(false, 64)],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    map.insert("ref", vec![
        Signature {
            arg_types: vec![Type::Generic("a")],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    map.insert("deref", vec![
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a")))],
            ret_type: Type::Generic("a"),
            index: None,
        },
        Signature {
            arg_types: vec![Type::Slice(true, Box::new(Type::Generic("a")))],
            ret_type: Type::Generic("a"),
            index: None,
        },
    ]);
    map.insert("slice", vec![
        Signature {
            arg_types: vec![Type::Int(false, 64), Type::Pointer(true, Box::new(Type::Generic("a")))],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    map.insert("syscall", vec![
        Signature {
            arg_types: vec![Type::Int(false, 64), Type::Int(false, 64), Type::Int(false, 64), Type::Int(false, 64), Type::Int(false, 64), Type::Int(false, 64), Type::Int(false, 64)],
            ret_type: Type::Int(false, 64),
            index: None,
        },
    ]);

    map
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

#[derive(Debug, Clone)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub generics: Vec<Type<'a>>,
    pub fields: Vec<(&'a str, Type<'a>)>,
}

pub fn extract_structs<'a>(sexprs: &[SExpr<'a>], map: &mut HashMap<&'a str, Struct<'a>>) {
    for sexpr in sexprs.iter() {
        if let SExpr::StructDef { name, fields, .. } = sexpr {
            let mut generics = vec![];

            for (_, t) in fields {
                t.find_generics(&mut generics);
            }

            let struct_ = Struct {
                name: *name,
                generics,
                fields: fields.clone(),
            };

            if map.insert(*name, struct_).is_some() {
                todo!("error handling");
            }
        }
    }
}
