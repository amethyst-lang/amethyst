use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    ops::Range,
};

use petgraph::{Graph, Undirected};

use super::ast_lowering::{LValue, SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {}

/*
#[derive(Debug)]
enum TypeConstraint<'a> {
    Int(Type<'a>),
    Float(Type<'a>),
    Equals(Type<'a>, Type<'a>),
}
*/

/*
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
                        if sig.has_generic() {
                            let mut map = HashMap::new();
                            sig.replace_generics(type_var_counter, &mut map, var_ranges);
                            monomorphisms.push((sig.clone(), index));
                        }
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
                                sig.replace_generics(type_var_counter, &mut map, var_ranges);
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
                        t.replace_generics(type_var_counter, &mut map, var_ranges);
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
                    var_ranges.push(0..0);
                    constraints.push(TypeConstraint::Equals(Type::TypeVariable(i), Type::Pointer(true, Box::new(t.clone()))));
                    Some(t)
                }

                _ => None,
            }
        }

        LValue::Get(v, i) => {
            create_constraints(&mut **i, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges);
            constraints.push(TypeConstraint::Equals(i.meta().type_.clone(), Type::Int(false, 64)));
            match create_lvalue_constraints(&mut **v, type_var_counter, constraints, func_map, struct_map, monomorphisms, scopes, var_ranges) {
                Some(Type::Slice(_, t)) => Some(*t),

                Some(Type::TypeVariable(i)) => {
                    let t = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    var_ranges.push(0..0);
                    constraints.push(TypeConstraint::Equals(Type::TypeVariable(i), Type::Slice(true, Box::new(t.clone()))));
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
                            let mut ranges = vec![];
                            obj = t.clone();
                            obj.replace_generics(&mut type_var_counter, &mut map, &mut ranges);
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

fn get_shared_type_variables_helper<'a, 'b>(mut t: &'b Type<'a>, set: &mut HashSet<u64>, substitutions: &'b [Type<'a>]) {
    match t {
        Type::Tuple(v) => {
            for v in v {
                get_shared_type_variables_helper(v, set, substitutions);
            }
        }

        Type::Pointer(_, v) => get_shared_type_variables_helper(&**v, set, substitutions),
        Type::Slice(_, v) => get_shared_type_variables_helper(&**v, set, substitutions),

        Type::Struct(_, g) => {
            for g in g {
                get_shared_type_variables_helper(g, set, substitutions);
            }
        }

        Type::Function(a, r) => {
            for a in a {
                get_shared_type_variables_helper(a, set, substitutions);
            }

            get_shared_type_variables_helper(&**r, set, substitutions);
        }

        Type::TypeVariable(_) => {
            let original = t;

            while let &Type::TypeVariable(v) = t {
                set.insert(v);
                let u = &substitutions[v as usize];
                if t == u {
                    break;
                }
                t = u;
            }

            let mut t = original.clone();
            loop {
                let mut not_found = true;
                for (i, sub) in substitutions.iter().enumerate() {
                    if *sub == t {
                        if let Type::TypeVariable(j) = t {
                            if i as u64 == j {
                                continue;
                            }
                        }
                        not_found = false;
                        set.insert(i as u64);
                        t = Type::TypeVariable(i as u64);
                        break;
                    }
                }

                if not_found {
                    break;
                }
            }
        }
        _ => (),
    }
}

fn get_shared_type_variables(t1: &Type, t2: &Type, substitutions: &[Type]) -> Vec<u64> {
    let mut a = HashSet::new();
    let mut b = HashSet::new();
    get_shared_type_variables_helper(t1, &mut a, substitutions);
    get_shared_type_variables_helper(t2, &mut b, substitutions);
    a.intersection(&b).cloned().collect()
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

    for (k, i) in unification_graph.node_indices().enumerate() {
        for j in unification_graph.node_indices().skip(k) {
            if i != j {
                let (t1, _) = unification_graph.node_weight(i).unwrap();
                let (t2, _) = unification_graph.node_weight(j).unwrap();
                let v = get_shared_type_variables(t1, t2, &substitutions);
                for v in v {
                    unification_graph.add_edge(i, j, v);
                }
            }
        }
    }

    if !unification_graph.raw_nodes().is_empty() {
        println!("{:?}", unification_graph.node_indices().map(|v| unification_graph.neighbors(v).count() + 1).collect::<Vec<_>>());
        let mut done = HashSet::new();
        for i in unification_graph.node_indices() {
            if done.contains(&i) {
                continue;
            }

            let connected: Vec<_> = unification_graph.neighbors(i).chain(once(i)).collect();
            done.extend(connected.iter().cloned());

            let mut selected = vec![0; connected.len()];
            let mut successes = vec![];

            'b: loop {
                let mut sub = substitutions.clone();
                let mut foi = float_or_int.clone();
                let mut success = true;

                for (i, index) in connected.iter().enumerate() {
                    let (t, options) = unification_graph.node_weight(*index).unwrap();
                    if unify(&mut sub, t, &options[selected[i]]).is_err() {
                        success = false;
                        break;
                    }
                }

                if success
                    && unify_type_variable_propagator(&mut sub, &mut foi).is_ok()
                    && check_float_or_int(&mut sub, &foi).is_ok()
                    && unify_type_variable_propagator(&mut sub, &mut foi).is_ok()
                    && unify_attrs(&attrs, &mut sub, struct_map).is_ok()
                {
                    successes.push((sub, foi));
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

            println!("length: {}", successes.len());
            if successes.len() == 1 {
                let (subs, foi) = successes.remove(0);
                substitutions = subs;
                float_or_int = foi;
            } else {
                println!("{:?}", connected.iter().map(|v| unification_graph.node_weight(*v).unwrap()).collect::<Vec<_>>());
                return Err(CorrectnessError::TooManyPossibilities);
            }
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

fn apply_lvalue_subs<'a>(lvalue: &mut LValue<'a>, substitutions: &[Type<'a>]) {
    match lvalue {
        LValue::Symbol(_) => (),
        LValue::Attribute(v, _) => apply_lvalue_subs(&mut **v, substitutions),
        LValue::Deref(v) => apply_lvalue_subs(&mut **v, substitutions),
        LValue::Get(v, i) => {
            apply_lvalue_subs(&mut **v, substitutions);
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
            apply_substitutions(value, substitutions);
            apply_lvalue_subs(lvalue, substitutions);
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
*/

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum FloatOrInt {
    Float,
    Int,
}

fn get_leaf<'a, 'b>(mut t: &'b Type<'a>, substitutions: &'b HashMap<u64, Type<'a>>) -> &'b Type<'a> {
    while let Type::TypeVariable(i) = t {
        if let Some(u) = substitutions.get(i) {
            t = u;
        } else {
            break;
        }
    }
    t
}

fn substitute<'a>(assignee: &mut Type<'a>, assigner: &Type<'a>, substitutions: &mut HashMap<u64, Type<'a>>, coercions: &mut HashMap<u64, FloatOrInt>) -> Result<(), CorrectnessError> {
    *assignee = get_leaf(assignee, substitutions).clone();
    let assigner = get_leaf(assigner, substitutions).clone();

    if assigner == *assignee {
        Ok(())
    } else if *assignee == Type::Unknown {
        *assignee = assigner.clone();
        Ok(())
    } else if let Type::TypeVariable(i) = assignee {
        match (coercions.get(i), &assigner) {
            (Some(FloatOrInt::Int), Type::Int(_, _)) => (),
            (Some(FloatOrInt::Float), Type::F32 | Type::F64) => (),
            (None, _) => (),
            _ => todo!("error handling"),
        }
        if substitutions.insert(*i, assigner.clone()).is_some() {
            todo!("error handling");
        } else {
            *assignee = assigner;
            Ok(())
        }
    } else if let Type::TypeVariable(i) = assigner {
        match (coercions.get(&i), &assignee) {
            (Some(FloatOrInt::Int), Type::Int(_, _)) => (),
            (Some(FloatOrInt::Float), Type::F32 | Type::F64) => (),
            (None, _) => (),
            _ => todo!("error handling"),
        }
        if substitutions.insert(i, assignee.clone()).is_some() {
            todo!("error handling");
        } else {
            Ok(())
        }
    } else if *assignee != assigner {
        todo!("error handling");
    } else {
        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
fn traverse_sexpr<'a, 'b>(
    sexpr: &'b mut SExpr<'a>,
    type_var_counter: &mut u64,
    substitutions: &mut HashMap<u64, Type<'a>>,
    coercions: &mut HashMap<u64, FloatOrInt>,
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
    monomorphisms: &mut Vec<(Type<'a>, usize)>,
    scopes: &mut Vec<HashMap<&'a str, Type<'a>>>,
    break_type: &mut Option<Type<'a>>,
) -> Result<(), CorrectnessError> {
    let t = &mut sexpr.meta_mut().type_;
    if *t == Type::UnknownInt {
        *t = Type::TypeVariable(*type_var_counter);
        coercions.insert(*type_var_counter, FloatOrInt::Int);
        *type_var_counter += 1;
    } else if *t == Type::UnknownFloat {
        *t = Type::TypeVariable(*type_var_counter);
        coercions.insert(*type_var_counter, FloatOrInt::Float);
        *type_var_counter += 1;
    }

    match sexpr {
        SExpr::Int { .. }
        | SExpr::Float { .. }
        | SExpr::Str { .. }
        | SExpr::Nil { .. } => Ok(()),

        SExpr::Symbol { meta, value } => {
            let mut var_type = None;
            for scope in scopes.iter_mut().rev() {
                if let Some(t) = scope.get_mut(value) {
                    var_type = Some(t);
                    break;
                }
            }

            if let Some(var_type) = var_type {
                meta.type_ = var_type.clone();
                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::List { .. } => todo!(),
        SExpr::Quote { .. } => todo!(),
        SExpr::Comma { .. } => todo!(),
        SExpr::Backtick { .. } => todo!(),
        SExpr::Splice { .. } => todo!(),

        SExpr::Seq { meta, values } => {
            scopes.push(HashMap::new());
            for value in values.iter_mut() {
                traverse_sexpr(value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            }

            if let Some(last) = values.last() {
                meta.type_ = last.meta().type_.clone();
            }

            scopes.pop();
            Ok(())
        }

        SExpr::Cond { meta, values, elsy } => {
            for (cond, then) in values.iter_mut() {
                scopes.push(HashMap::new());
                traverse_sexpr(cond, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
                traverse_sexpr(then, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;

                match &cond.meta().type_ {
                    Type::Int(_, _) => (),

                    Type::TypeVariable(v) => {
                        if let Some(t) = substitutions.get(v) {
                            if !matches!(t, Type::Int(_, _)) {
                                todo!("error handling");
                            }
                        } else if let Some(FloatOrInt::Int) = coercions.get(v) {
                            substitutions.insert(*v, Type::Int(true, 32));
                        } else {
                            todo!("error handling");
                        }
                    }

                    _ => todo!("error handling"),
                }

                substitute(&mut meta.type_, &then.meta().type_, substitutions, coercions)?;
                scopes.pop();
            }

            if let Some(elsy) = elsy {
                scopes.push(HashMap::new());
                traverse_sexpr(&mut **elsy, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
                substitute(&mut meta.type_, &elsy.meta().type_, substitutions, coercions)?;
                scopes.pop();
            } else if meta.type_ != Type::Tuple(vec![]) {
                todo!("error handling");
            }

            Ok(())
        }

        SExpr::Loop { meta, value } => {
            scopes.push(HashMap::new());
            let mut break_type = Some(Type::Unknown);
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, &mut break_type)?;

            meta.type_ = match break_type {
                Some(Type::Unknown) => Type::Tuple(vec![]),
                Some(v) => v,
                None => unreachable!(),
            };
            scopes.pop();

            Ok(())
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
                if let Some(type_) = break_type {
                    substitute(type_, &value.meta().type_, substitutions, coercions)
                } else {
                    todo!("error handling");
                }
            } else if let Some(type_) = break_type {
                substitute(type_, &Type::Tuple(vec![]), substitutions, coercions)
            } else {
                todo!("error handling");
            }
        }

        SExpr::Type { meta, value } => {
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            substitute(&mut meta.type_, &value.meta().type_, substitutions, coercions)
        }

        SExpr::FuncDef { .. } => Ok(()),

        SExpr::FuncCall { meta, func, values } => todo!(),
        SExpr::StructDef { meta, name, fields } => todo!(),
        SExpr::StructSet { meta, name, values } => todo!(),

        SExpr::Declare { meta, variable, value, .. } => {
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            meta.type_ = value.meta().type_.clone();
            scopes.last_mut().unwrap().insert(*variable, meta.type_.clone());
            Ok(())
        }

        SExpr::Assign { meta, lvalue: LValue::Symbol(variable), value } => {
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;

            let mut var_type = None;
            for scope in scopes.iter_mut().rev() {
                if let Some(t) = scope.get_mut(variable) {
                    var_type = Some(t);
                    break;
                }
            }

            if let Some(var_type) = var_type {
                substitute(var_type, &value.meta().type_, substitutions, coercions)?;
                meta.type_ = var_type.clone();
                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::Assign { meta, lvalue, value } => todo!(),

        SExpr::Attribute { meta, top, attrs } => todo!(),
    }
}

fn apply_substitutions<'a>(sexpr: &mut SExpr<'a>, substitutions: &HashMap<u64, Type<'a>>) {
    if let Type::TypeVariable(i) = sexpr.meta().type_ {
        if let Some(mut t) = substitutions.get(&i) {
            while let Type::TypeVariable(i) = t {
                if let Some(v) = substitutions.get(i) {
                    t = v;
                } else {
                    todo!("error handling");
                }
            }

            sexpr.meta_mut().type_ = t.clone();
        } else {
            todo!("error handling");
        }
    }

    match sexpr {
        SExpr::List { .. } => todo!(),
        SExpr::Quote { .. } => todo!(),
        SExpr::Comma { .. } => todo!(),
        SExpr::Backtick { .. } => todo!(),
        SExpr::Splice { .. } => todo!(),

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

        SExpr::Loop { value, .. } => apply_substitutions(&mut **value, substitutions),
        SExpr::Break { value: Some(value), .. } => apply_substitutions(&mut **value, substitutions),

        SExpr::Type { value, .. } => apply_substitutions(&mut **value, substitutions),

        SExpr::FuncCall { func, values, .. } => {
            apply_substitutions(&mut **func, substitutions);
            for value in values {
                apply_substitutions(value, substitutions);
            }
        }

        SExpr::StructSet { values, .. } => {
            for (_, value) in values {
                apply_substitutions(value, substitutions);
            }
        }

        SExpr::Declare { value, .. } => apply_substitutions(&mut **value, substitutions),
        SExpr::Assign { value, .. } => apply_substitutions(&mut **value, substitutions),
        SExpr::Attribute { top, .. } => apply_substitutions(&mut **top, substitutions),

        _ => (),
    }
}

pub fn check<'a>(
    original: &'a str,
    sexprs: &mut Vec<SExpr<'a>>,
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
) -> Result<(), CorrectnessError> {
    let mut skip = 0;

    while {
        let mut monomorphisms = vec![];

        for sexpr in sexprs.iter_mut().skip(skip) {
            if let SExpr::FuncDef { ret_type, args, expr, .. } = sexpr {
                if args.iter().any(|(_, v)| v.has_generic()) || ret_type.has_generic() {
                    continue;
                }

                let mut scopes = vec![HashMap::new()];
                for (arg, type_) in args {
                    scopes.last_mut().unwrap().insert(*arg, type_.clone());
                }

                let mut type_var_counter = 0;
                let mut substitutions = HashMap::new();
                let mut coercions = HashMap::new();

                traverse_sexpr(
                    expr,
                    &mut type_var_counter,
                    &mut substitutions,
                    &mut coercions,
                    func_map,
                    struct_map,
                    &mut monomorphisms,
                    &mut scopes,
                    &mut None,
                )?;

                for (i, coercion) in coercions {
                    match substitutions.entry(i) {
                        Entry::Occupied(_) => (),
                        Entry::Vacant(v) => {
                            match coercion {
                                FloatOrInt::Float => v.insert(Type::F64),
                                FloatOrInt::Int => v.insert(Type::Int(true, 32)),
                            };
                        }
                    }
                }

                apply_substitutions(expr, &substitutions);
            }
        }

        /*
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
        */

        skip = sexprs.len();

        if !monomorphisms.is_empty() {
            /*
            let mut done = HashSet::new();
            for (mut t, index) in monomorphisms {
                let mut monomorphised = sexprs[index].clone();

                if let SExpr::FuncDef { meta, args, ret_type, .. } = &mut monomorphised {
                    //flatten_substitution(&mut t, &unified);
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
            */

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

fn create_one_signature<'a>(
    map: &mut HashMap<&'a str, Vec<Signature<'a>>>,
    name: &'a str,
    include_floats: bool,
    rets_bool: bool,
) {
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
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(true, 32),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(true, 64),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 32),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 64),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    ]);
    create_one_signature(&mut map, "-", true, false);
    map.get_mut("-").unwrap().extend([
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(true, 32),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(true, 64),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 32),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 64),
            ],
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
    map.insert(
        "and",
        vec![Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        }],
    );
    map.insert(
        "or",
        vec![Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        }],
    );
    map.insert(
        "xor",
        vec![Signature {
            arg_types: vec![Type::Int(false, 1), Type::Int(false, 1)],
            ret_type: Type::Int(false, 1),
            index: None,
        }],
    );

    let mut sigs = vec![];
    for a in [
        Type::Int(false, 8),
        Type::Int(false, 16),
        Type::Int(false, 32),
        Type::Int(false, 64),
        Type::Int(true, 8),
        Type::Int(true, 16),
        Type::Int(true, 32),
        Type::Int(true, 64),
    ] {
        for r in [
            Type::Int(false, 8),
            Type::Int(false, 16),
            Type::Int(false, 32),
            Type::Int(false, 64),
            Type::Int(true, 8),
            Type::Int(true, 16),
            Type::Int(true, 32),
            Type::Int(true, 64),
        ] {
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
    map.insert(
        "alloca",
        vec![
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
        ],
    );
    map.insert(
        "ref",
        vec![Signature {
            arg_types: vec![Type::Generic("a")],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        }],
    );
    map.insert(
        "deref",
        vec![Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a")))],
            ret_type: Type::Generic("a"),
            index: None,
        }],
    );
    map.insert(
        "get",
        vec![Signature {
            arg_types: vec![
                Type::Slice(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 64),
            ],
            ret_type: Type::Generic("a"),
            index: None,
        }],
    );
    map.insert(
        "slice",
        vec![Signature {
            arg_types: vec![
                Type::Int(false, 64),
                Type::Pointer(true, Box::new(Type::Generic("a"))),
            ],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a"))),
            index: None,
        }],
    );
    map.insert(
        "syscall",
        vec![Signature {
            arg_types: vec![
                Type::Int(false, 64),
                Type::Int(false, 64),
                Type::Int(false, 64),
                Type::Int(false, 64),
                Type::Int(false, 64),
                Type::Int(false, 64),
                Type::Int(false, 64),
            ],
            ret_type: Type::Int(false, 64),
            index: None,
        }],
    );

    map
}

pub fn extract_signatures<'a>(
    sexprs: &[SExpr<'a>],
    map: &mut HashMap<&'a str, Vec<Signature<'a>>>,
) {
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
                        index,
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
