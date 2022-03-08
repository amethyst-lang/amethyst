use std::{collections::{HashMap, hash_map::Entry, HashSet}, ops::Range};

use petgraph::{Graph, Undirected, graph::NodeIndex};

use super::ast_lowering::{SExpr, Type, LValue};

#[derive(Debug)]
pub enum CorrectnessError<'a> {
    OccursError(Type<'a>, Type<'a>),
    LengthMismatch(Type<'a>, Type<'a>),
    TypeError(Type<'a>, Type<'a>),
    FloatOrIntError(FloatOrInt, FloatOrInt),
    MismatchedFloatOrInt(Type<'a>, FloatOrInt),
    UnassignedTypeVariable(Type<'a>),
    InvalidAttr(Type<'a>, &'a str),
    TooManyPossibilities,

}

#[derive(Debug)]
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

#[allow(clippy::too_many_arguments)]
fn create_constraints<'a>(
    sexpr: &mut SExpr<'a>,
    type_var_counter: &mut u64,
    constraints: &mut Vec<TypeConstraint<'a>>,
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
    monomorphisms: &mut Vec<(Type<'a>, usize)>,
    scopes: &mut Vec<HashMap<&'a str, Type<'a>>>,
    var_ranges: &mut Vec<Range<usize>>,
) {
    let meta = sexpr.meta_mut();
    if meta.type_ == Type::Unknown {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        *type_var_counter += 1;
        var_ranges.push(meta.range.clone());
    } else if meta.type_ == Type::UnknownInt {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        constraints.push(TypeConstraint::Int(meta.type_.clone()));
        *type_var_counter += 1;
        var_ranges.push(meta.range.clone());
    } else if meta.type_ == Type::UnknownFloat {
        meta.type_ = Type::TypeVariable(*type_var_counter);
        constraints.push(TypeConstraint::Float(meta.type_.clone()));
        *type_var_counter += 1;
        var_ranges.push(meta.range.clone());
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
                if v.len() == 1 {
                    if let Some(index) = v[0].index {
                        let mut sig = Type::Function(v[0].arg_types.clone(), Box::new(v[0].ret_type.clone()));
                        let mut map = HashMap::new();
                        sig.replace_generics(type_var_counter, &mut map);
                        monomorphisms.push((sig.clone(), index));
                        constraints.push(TypeConstraint::Equals(meta.type_.clone(), sig));
                    }
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
                create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
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
                create_constraints(cond, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
                constraints.push(TypeConstraint::Int(cond.meta().type_.clone()));
                create_constraints(then, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
                scopes.pop();

                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    then.meta().type_.clone(),
                ));
            }

            if let Some(elsy) = elsy {
                scopes.push(HashMap::new());
                create_constraints(elsy, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);

                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    elsy.meta().type_.clone(),
                ));
                scopes.pop();
            }
        }

        SExpr::Loop { meta, value } => {
            scopes.push(HashMap::new());
            create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            create_loop_constraints(&meta.type_, value, constraints);
            constraints.push(TypeConstraint::Nillable(meta.type_.clone()));
            scopes.pop();
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            }
        }

        SExpr::Nil { .. } => (),

        SExpr::Type { meta, value } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
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
            create_constraints(&mut **expr, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            constraints.push(TypeConstraint::Equals(ret_type.clone(), expr.meta().type_.clone()));
            scopes.pop();
        }

        SExpr::FuncCall { meta, func, values } => {
            create_constraints(&mut **func, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            let mut args = vec![];
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
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
                                if let Some(index) = v.index {
                                    monomorphisms.push((sig.clone(), index));
                                }
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
                    create_constraints(value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);

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
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            if let Some(scope) = scopes.last_mut() {
                scope.insert(*variable, value.meta().type_.clone());
            }
            constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
        }

        SExpr::Assign {
            meta,
            lvalue,
            value,
        } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            if let Some(t) = create_lvalue_constraints(lvalue, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges) {
                constraints.push(TypeConstraint::Equals(t, value.meta().type_.clone()));
                constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
            }
        }

        SExpr::Attribute { meta, top, attrs } => {
            create_constraints(&mut **top, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            constraints.push(TypeConstraint::Attribute(meta.type_.clone(), top.meta().type_.clone(), attrs.clone()));
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn create_lvalue_constraints<'a>(
    lvalue: &mut LValue<'a>,
    type_var_counter: &mut u64,
    constraints: &mut Vec<TypeConstraint<'a>>,
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
    monomorphisms: &mut Vec<(Type<'a>, usize)>,
    scopes: &mut Vec<HashMap<&'a str, Type<'a>>>,
    var_ranges: &mut Vec<Range<usize>>,
) -> Option<Type<'a>> {
    match lvalue {
        LValue::Symbol(sym) => {
            for scope in scopes.iter().rev() {
                if let Some(t) = scope.get(sym) {
                    return Some(t.clone());
                }
            }

            None
        }

        LValue::Attribute(_, _) => todo!(),

        LValue::Deref(v) => {
            match create_lvalue_constraints(&mut **v, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges) {
                Some(Type::Pointer(_, t)) => Some(*t),

                Some(Type::TypeVariable(i)) => {
                    let t = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    constraints.push(TypeConstraint::Equals(Type::TypeVariable(i), Type::Pointer(true, Box::new(t.clone()))));
                    Some(t)
                }

                _ => None,
            }
        }

        LValue::Get(typ, v, i) => {
            create_constraints(&mut **i, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            constraints.push(TypeConstraint::Equals(i.meta().type_.clone(), Type::Int(false, 64)));
            match create_lvalue_constraints(&mut **v, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges) {
                Some(Type::Slice(_, t)) => {
                    *typ = (*t).clone();
                    Some(*t)
                }

                Some(Type::TypeVariable(i)) => {
                    let t = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    var_ranges.push(0..0);
                    constraints.push(TypeConstraint::Equals(Type::TypeVariable(i), Type::Slice(true, Box::new(t.clone()))));
                    *typ = t.clone();
                    Some(t)
                }

                _ => None,
            }
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

fn unify<'a>(substitutions: &mut Vec<Type<'a>>, t1: &Type<'a>, t2: &Type<'a>) -> Result<(), CorrectnessError<'a>> {
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
                return Err(CorrectnessError::OccursError(Type::TypeVariable(*i), t2.clone()));
            }

            substitutions[*i as usize] = t2.clone();
            Ok(())
        }

        (t1, Type::TypeVariable(i)) => {
            if occurs_in(substitutions, *i, t1) {
                return Err(CorrectnessError::OccursError(t1.clone(), Type::TypeVariable(*i)));
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
                return Err(CorrectnessError::LengthMismatch(Type::Tuple(v1.clone()), Type::Tuple(v2.clone())));
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
                return Err(CorrectnessError::LengthMismatch(Type::Struct(*name1, v1.clone()), Type::Struct(*name2, v2.clone())));
            }

            for (t1, t2) in v1.iter().zip(v2) {
                unify(substitutions, t1, t2)?;
            }

            Ok(())
        }

        (Type::Generic(_), Type::Generic(_)) => Ok(()),

        (Type::Function(v1, t1), Type::Function(v2, t2)) => {
            if v1.len() != v2.len() {
                return Err(CorrectnessError::LengthMismatch(Type::Function(v1.clone(), t1.clone()), Type::Function(v2.clone(), t2.clone())));
            }

            for (t1, t2) in v1.iter().zip(v2) {
                unify(substitutions, t1, t2)?;
            }

            unify(substitutions, &**t1, &**t2)
        }

        _ => Err(CorrectnessError::TypeError(t1.clone(), t2.clone())),
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatOrInt {
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

fn unify_type_variable_propagator<'a>(substitutions: &mut Vec<Type<'a>>, float_or_int: &mut Vec<FloatOrInt>) -> Result<(), CorrectnessError<'a>> {
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
                _ => return Err(CorrectnessError::FloatOrIntError(float_or_int[i], float_or_int[j])),
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

fn check_float_or_int<'a>(substitutions: &mut Vec<Type<'a>>, float_or_int: &[FloatOrInt]) -> Result<(), CorrectnessError<'a>> {
    for (i, foi) in float_or_int.iter().enumerate() {
        match foi {
            FloatOrInt::Float => match substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::F64;
                }

                Type::F32 | Type::F64 => (),

                _ => return Err(CorrectnessError::MismatchedFloatOrInt(substitutions[i].clone(), *foi)),
            },

            FloatOrInt::Int => match substitutions[i] {
                Type::TypeVariable(_) => {
                    substitutions[i] = Type::Int(true, 32);
                }

                Type::Int(_, _) => (),

                _ => return Err(CorrectnessError::MismatchedFloatOrInt(substitutions[i].clone(), *foi)),
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

fn unify_attrs<'a>(attrs: &[(Type<'a>, Type<'a>, Vec<&'a str>)], substitutions: &mut Vec<Type<'a>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<(), CorrectnessError<'a>> {
    for (result, obj, attrs) in attrs {
        let mut obj = obj.clone();
        while let Type::TypeVariable(i) = obj {
            let new = substitutions[i as usize].clone();
            match new {
                Type::TypeVariable(j) if i == j => return Err(CorrectnessError::UnassignedTypeVariable(Type::TypeVariable(i))),
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
                            return Err(CorrectnessError::InvalidAttr(Type::Struct(name, generics), *attr));
                        }
                    } else {
                        return Err(CorrectnessError::InvalidAttr(Type::Struct(name, generics), *attr));
                    }
                }

                Type::Slice(mutable, t) => {
                    match *attr {
                        "len" | "cap" => obj = Type::Int(false, 64),
                        "ptr" => obj = Type::Pointer(mutable, t),
                        _ => return Err(CorrectnessError::InvalidAttr(Type::Slice(mutable, t), *attr)),
                    }
                }

                Type::TypeVariable(_) => (),

                _ => return Err(CorrectnessError::InvalidAttr(obj.clone(), *attr))
            }
        }

        match (result, obj) {
            (Type::TypeVariable(i), Type::TypeVariable(j)) if *i == j => return Err(CorrectnessError::UnassignedTypeVariable(Type::TypeVariable(j))),

            (_, Type::TypeVariable(_)) => (),

            (Type::TypeVariable(i), t) => {
                match &substitutions[*i as usize] {
                    Type::TypeVariable(j) if *i == *j => substitutions[*i as usize] = t,
                    _ => unify(substitutions, result, &t)?,
                }
            }

            (a, b) if *a == b => (),

            (_, obj) => return Err(CorrectnessError::TypeError(result.clone(), obj)),
        }
    }

    Ok(())
}

fn get_shared_type_variables_helper(t: &Type, set: &mut HashSet<u64>) {
    match t {
        Type::Tuple(v) => {
            for v in v {
                get_shared_type_variables_helper(v, set);
            }
        }

        Type::Pointer(_, v) => get_shared_type_variables_helper(&**v, set),
        Type::Slice(_, v) => get_shared_type_variables_helper(&**v, set),

        Type::Struct(_, g) => {
            for g in g {
                get_shared_type_variables_helper(g, set);
            }
        }

        Type::Function(a, r) => {
            for a in a {
                get_shared_type_variables_helper(a, set);
            }

            get_shared_type_variables_helper(&**r, set);
        }

        Type::TypeVariable(v) => {
            set.insert(*v);
        }
        _ => (),
    }
}

fn get_shared_type_variables(t1: &Type, t2: &Type) -> Vec<u64> {
    let mut a = HashSet::new();
    let mut b = HashSet::new();
    get_shared_type_variables_helper(t1, &mut a);
    get_shared_type_variables_helper(t2, &mut b);
    a.intersection(&b).cloned().collect()
}

fn unify_get_connected_nodes<'a>(unification_graph: &Graph<(Type<'a>, Vec<Type<'a>>), u64, Undirected, u32>, index: NodeIndex) -> Vec<NodeIndex> {
    let mut result = HashSet::new();
    let mut stack = vec![index];

    while let Some(i) = stack.pop() {
        if !result.insert(i) {
            let neighbours = unification_graph.neighbors(i);
            stack.extend(neighbours);
        }
    }

    result.into_iter().collect()
}

fn unify_types<'a>(type_var_counter: u64, constraints: Vec<TypeConstraint<'a>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<Vec<Type<'a>>, CorrectnessError<'a>> {
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
                t => return Err(CorrectnessError::MismatchedFloatOrInt(t, FloatOrInt::Int))
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
                t => return Err(CorrectnessError::MismatchedFloatOrInt(t, FloatOrInt::Float))
            },

            TypeConstraint::Equals(t1, t2) => if unify(&mut substitutions, &t1, &t2).is_err() {
                return Err(CorrectnessError::TypeError(t1, t2))
            },

            TypeConstraint::Nillable(t) => match t {
                Type::TypeVariable(i)
                    if matches!(
                        float_or_int[i as usize],
                        FloatOrInt::Nil | FloatOrInt::Neither
                    ) => float_or_int[i as usize] = FloatOrInt::Nil,
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

    let mut unification_graph: Graph<_, _, Undirected, u32> = Graph::default();

    for v in unification_options {
        unification_graph.add_node(v);
    }

    for i in unification_graph.node_indices() {
        for j in unification_graph.node_indices() {
            if i != j {
                let (t1, v1) = unification_graph.node_weight(i).unwrap().clone();
                let (t2, v2) = unification_graph.node_weight(j).unwrap().clone();
                let v = get_shared_type_variables(&t1, &t2);
                for v in v {
                    unification_graph.add_edge(i, j, v);
                }
                for v in v1 {
                    let v = get_shared_type_variables(&t1, &v);
                    for v in v {
                        unification_graph.add_edge(i, j, v);
                    }
                }
                for v in v2 {
                    let v = get_shared_type_variables(&t1, &v);
                    for v in v {
                        unification_graph.add_edge(i, j, v);
                    }
                }
            }
        }
    }

    if !unification_graph.raw_nodes().is_empty() {
        for i in unification_graph.node_indices() {
            let connected = unify_get_connected_nodes(&unification_graph, i);
            let mut selected = vec![0; connected.len()];
            let mut successes = vec![];

            'b: loop {
                let mut sub = substitutions.clone();
                let mut success = true;

                for (i, index) in connected.iter().enumerate() {
                    let (t, options) = unification_graph.node_weight(*index).unwrap();
                    if unify(&mut sub, t, &options[selected[i]]).is_err() {
                        success = false;
                        break;
                    }
                }

                if success && unify_attrs(&attrs, &mut sub, struct_map).is_ok() {
                    successes.push(sub);
                }

                let last_index = selected.len() - 1;
                for (i, selection) in selected.iter_mut().enumerate() {
                    *selection += 1;

                    if unification_graph.node_weight(connected[i]).unwrap().1.len() > *selection {
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
            } else {
                return Err(CorrectnessError::TooManyPossibilities);
            }
        }

        unify_type_variable_propagator(&mut substitutions, &mut float_or_int)?;
        check_float_or_int(&mut substitutions, &float_or_int)?;
        unify_type_variable_propagator(&mut substitutions, &mut float_or_int)?;
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

fn apply_subs_lvalue<'a>(lvalue: &mut LValue<'a>, substitutions: &[Type<'a>]) {
    match lvalue {
        LValue::Symbol(_) => (),
        LValue::Attribute(v, _) => apply_subs_lvalue(&mut **v, substitutions),
        LValue::Deref(v) => apply_subs_lvalue(&mut **v, substitutions),
        LValue::Get(t, v, i) => {
            flatten_substitution(t, substitutions);
            apply_subs_lvalue(&mut **v, substitutions);
            apply_substitutions(&mut **i, substitutions);
        }
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
            | SExpr::Break { value: Some(value), .. } => apply_substitutions(value, substitutions),

        SExpr::Assign { lvalue, value, .. } => {
            apply_subs_lvalue(lvalue, substitutions);
            apply_substitutions(value, substitutions);
        }


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

pub fn check<'a>(original: &'a str, sexprs: &mut Vec<SExpr<'a>>, func_map: &HashMap<&'a str, Vec<Signature<'a>>>, struct_map: &HashMap<&'a str, Struct<'a>>) -> Result<(), CorrectnessError<'a>> {
    let mut monomorphisms;
    let mut skip = 0;
    let mut scopes = vec![HashMap::new()];

    while {
        let mut type_var_counter = 0;
        let mut constraints = vec![];
        let mut var_ranges = vec![];
        monomorphisms = vec![];

        for sexpr in sexprs.iter_mut().skip(skip) {
            create_constraints(sexpr, &mut type_var_counter, &mut constraints, func_map, struct_map, &mut monomorphisms, &mut scopes, &mut var_ranges);
        }

        for (i, var_range) in var_ranges.into_iter().enumerate() {
            println!("{} ({:?}):\n{}\n", i, var_range, &original[var_range.clone()]);
        }
        let unified = unify_types(type_var_counter, constraints, struct_map)?;
        for sexpr in sexprs.iter_mut().skip(skip) {
            apply_substitutions(sexpr, &unified);
        }

        skip = sexprs.len();

        if !monomorphisms.is_empty() {
            let mut done = HashSet::new();
            for (mut t, index) in monomorphisms {
                let mut monomorphised = sexprs[index].clone();

                if let SExpr::FuncDef { meta, args, ret_type, .. } = &mut monomorphised {
                    flatten_substitution(&mut t, &unified);
                    if done.contains(&t) {
                        continue;
                    }

                    done.insert(t.clone());
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
    ]);
    map.insert("get", vec![
        Signature {
            arg_types: vec![Type::Slice(true, Box::new(Type::Generic("a"))), Type::Int(false, 64)],
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
