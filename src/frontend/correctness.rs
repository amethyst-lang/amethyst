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

        SExpr::FuncCall { func, values, .. } => {
            create_loop_constraints(type_, &mut **func, constraints);
            for value in values.iter_mut() {
                create_loop_constraints(type_, value, constraints);
            }
        }

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
    func_map: &HashMap<&'a str, Vec<Signature<'a>>>,
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
                create_constraints(value, type_var_counter, constraints, func_map, monomorphisms, scopes);
            }
            scopes.pop();

            constraints.push(TypeConstraint::Equals(
                meta.type_.clone(),
                values.last().unwrap().meta().type_.clone(),
            ));
        }

        SExpr::Cond { meta, values } => {
            for (cond, then) in values {
                scopes.push(HashMap::new());
                create_constraints(cond, type_var_counter, constraints, func_map, monomorphisms, scopes);
                constraints.push(TypeConstraint::Int(cond.meta().type_.clone()));
                create_constraints(then, type_var_counter, constraints, func_map, monomorphisms, scopes);
                scopes.pop();

                constraints.push(TypeConstraint::Equals(
                    meta.type_.clone(),
                    then.meta().type_.clone(),
                ));
            }
        }

        SExpr::Loop { meta, value } => {
            scopes.push(HashMap::new());
            create_constraints(value, type_var_counter, constraints, func_map, monomorphisms, scopes);
            create_loop_constraints(&meta.type_, value, constraints);
            constraints.push(TypeConstraint::Nillable(meta.type_.clone()));
            scopes.pop();
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                create_constraints(&mut **value, type_var_counter, constraints, func_map, monomorphisms, scopes);
            }
        }

        SExpr::Nil { .. } => (),

        SExpr::Type { meta, value } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, monomorphisms, scopes);
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
            create_constraints(&mut **expr, type_var_counter, constraints, func_map, monomorphisms, scopes);
            constraints.push(TypeConstraint::Equals(ret_type.clone(), expr.meta().type_.clone()));
            scopes.pop();
        }

        SExpr::FuncCall { meta, func, values } => {
            create_constraints(&mut **func, type_var_counter, constraints, func_map, monomorphisms, scopes);
            let mut args = vec![];
            for value in values.iter_mut() {
                create_constraints(value, type_var_counter, constraints, func_map, monomorphisms, scopes);
                args.push(value.meta().type_.clone());
            }
            let func_type_var = Type::Function(args, Box::new(meta.type_.clone()));

            constraints.push(TypeConstraint::Equals(func.meta().type_.clone(), func_type_var.clone()));

            if let SExpr::Symbol { value, .. } = **func {
                if let Some(signatures) = func_map.get(value) {
                    let mut valid_sigs: Vec<_> = signatures.iter().filter(|v| v.arg_types.len() == values.len()).cloned().collect();

                    if !valid_sigs.is_empty() {
                        if valid_sigs.len() == 1 {
                            let Signature { arg_types, ret_type, index } = valid_sigs.remove(0);
                            let mut sig = Type::Function(arg_types, Box::new(ret_type));
                            if sig.has_generic() {
                                let mut map = HashMap::new();
                                sig.replace_generics(type_var_counter, &mut map);

                                if let Some(index) = index {
                                    monomorphisms.push((sig.clone(), index));
                                }
                            }
                            constraints.push(TypeConstraint::Equals(func_type_var, sig));
                        } else {
                            constraints.push(TypeConstraint::Options(func_type_var, valid_sigs.into_iter().map(|v| Type::Function(v.arg_types, Box::new(v.ret_type))).collect()));
                        }
                    }
                }
            }
        }

        SExpr::StructDef { meta, name, fields } => todo!(),
        SExpr::StructSet {
            meta,
            struct_name,
            values,
        } => todo!(),

        SExpr::Declare {
            meta,
            mutable: _,
            variable,
            value,
        } => {
            create_constraints(&mut **value, type_var_counter, constraints, func_map, monomorphisms, scopes);
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
            create_constraints(&mut **value, type_var_counter, constraints, func_map, monomorphisms, scopes);
            for scope in scopes.iter().rev() {
                if let Some(t) = scope.get(variable) {
                    constraints.push(TypeConstraint::Equals(value.meta().type_.clone(), t.clone()));
                    constraints.push(TypeConstraint::Equals(meta.type_.clone(), value.meta().type_.clone()));
                }
            }
        }

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

#[derive(Copy, Clone, PartialEq)]
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

fn unify_types(type_var_counter: u64, constraints: Vec<TypeConstraint<'_>>) -> Result<Vec<Type<'_>>, CorrectnessError> {
    let mut substitutions = vec![Type::Unknown; type_var_counter as usize];
    let mut float_or_int = vec![FloatOrInt::Neither; type_var_counter as usize];
    let mut unification_options = vec![];

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
        }
    }

    if !unification_options.is_empty() {
        let mut selected = vec![0; unification_options.len()];
        let mut successes = vec![];

        'a: loop {
            let mut sub = substitutions.clone();
            let mut success = true;
            for (i, (t, options)) in unification_options.iter().enumerate() {
                if unify(&mut sub, t, &options[selected[i]]).is_err() {
                    success = false;
                    break;
                }
            }

            if success && check_float_or_int(&mut sub, &float_or_int).is_ok() {
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
                        break 'a;
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
    }

    for sub in substitutions.iter() {
        if let Type::TypeVariable(_) = sub {
            return Err(CorrectnessError::UnificationError);
        }
    }

    Ok(substitutions)
}

fn flatten_substitution<'a>(t: &mut Type<'a>, substitutions: &[Type<'a>]) {
    while let Type::TypeVariable(i) = t {
        *t = substitutions[*i as usize].clone();
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
    if let Type::TypeVariable(i) = sexpr.meta().type_ {
        sexpr.meta_mut().type_ = substitutions[i as usize].clone();
        //flatten_substitution(&mut sexpr.meta_mut().type_, substitutions);
    }

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

        SExpr::Cond { values, .. } => {
            for (cond, then) in values {
                apply_substitutions(cond, substitutions);
                apply_substitutions(then, substitutions);
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

        //SExpr::StructDef { meta, name, fields } => todo!(),
        //SExpr::StructSet { meta, struct_name, values } => todo!(),
        SExpr::Attribute { top, .. } => apply_substitutions(top, substitutions),
        _ => (),
    }
}

pub fn check<'a>(sexprs: &mut Vec<SExpr<'a>>, func_map: &HashMap<&'a str, Vec<Signature<'a>>>) -> Result<(), CorrectnessError> {
    let mut monomorphisms;
    let mut skip = 0;
    let mut scopes = vec![HashMap::new()];

    while {
        let mut type_var_counter = 0;
        let mut constraints = vec![];
        monomorphisms = vec![];

        for sexpr in sexprs.iter_mut().skip(skip) {
            create_constraints(sexpr, &mut type_var_counter, &mut constraints, func_map, &mut monomorphisms, &mut scopes);
        }

        let unified = unify_types(type_var_counter, constraints)?;
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
