use std::collections::{hash_map::Entry, HashMap, HashSet};

use super::ast_lowering::{LValue, SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {}

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
            (Some(&v), Type::TypeVariable(j)) => {
                match coercions.entry(*j) {
                    Entry::Occupied(w) => {
                        if *w.get() != v {
                            todo!("error handling");
                        }
                    }

                    Entry::Vacant(w) => {
                        w.insert(v);
                    }
                }
            }
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
            (Some(&v), Type::TypeVariable(j)) => {
                match coercions.entry(*j) {
                    Entry::Occupied(w) => {
                        if *w.get() != v {
                            todo!("error handling");
                        }
                    }

                    Entry::Vacant(w) => {
                        w.insert(v);
                    }
                }
            }
            (None, _) => (),
            _ => todo!("error handling"),
        }
        if substitutions.insert(i, assignee.clone()).is_some() {
            todo!("error handling");
        } else {
            Ok(())
        }
    } else {
        match (assignee, assigner) {
            (Type::Tuple(v1), Type::Tuple(v2)) => {
                if v1.len() != v2.len() {
                    todo!("error handling");
                }

                for (a, b) in v1.iter_mut().zip(v2) {
                    substitute(a, &b, substitutions, coercions)?;
                }

                Ok(())
            }

            (Type::Pointer(_, a), Type::Pointer(_, b)) => substitute(&mut **a, &*b, substitutions, coercions),
            (Type::Slice(_, a), Type::Slice(_, b)) => substitute(&mut **a, &*b, substitutions, coercions),

            (Type::Struct(_, v1), Type::Struct(_, v2)) => {
                if v1.len() != v2.len() {
                    todo!("error handling");
                }

                for (a, b) in v1.iter_mut().zip(v2) {
                    substitute(a, &b, substitutions, coercions)?;
                }

                Ok(())
            }

            (Type::Function(a1, r1), Type::Function(a2, r2)) => {
                if a1.len() != a2.len() {
                    todo!("error handling");
                }

                for (a, b) in a1.iter_mut().zip(a2) {
                    substitute(a, &b, substitutions, coercions)?;
                }

                substitute(&mut **r1, &*r2, substitutions, coercions)
            }

            (a, b) if *a == b => Ok(()),
            _ => todo!("error handling"),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn traverse_sexpr<'a>(
    sexpr: &mut SExpr<'a>,
    type_var_counter: &mut u64,
    substitutions: &mut HashMap<u64, Type<'a>>,
    coercions: &mut HashMap<u64, FloatOrInt>,
    func_map: &HashMap<&'a str, Signature<'a>>,
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
                substitute(&mut meta.type_, var_type, substitutions, coercions)?;
                Ok(())
            } else if let Some(func) = func_map.get(value) {
                substitute(&mut meta.type_, &Type::Function(func.arg_types.clone(), Box::new(func.ret_type.clone())), substitutions, coercions)?;
                if meta.type_.has_generic() {
                    let mut map = HashMap::new();
                    meta.type_.replace_generics(type_var_counter, &mut map);
                    if let Some(index) = func.index {
                        monomorphisms.push((meta.type_.clone(), index));
                    }
                }
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
                substitute(&mut meta.type_, &last.meta().type_, substitutions, coercions)?;
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

            let t = match break_type {
                Some(Type::Unknown) => Type::Tuple(vec![]),
                Some(v) => v,
                None => unreachable!(),
            };
            substitute(&mut meta.type_, &t, substitutions, coercions)?;
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

        SExpr::FuncCall { meta, func, values } => {
            traverse_sexpr(&mut **func, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;

            for value in values.iter_mut() {
                traverse_sexpr(value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            }

            if let Type::Function(arg_types, ret_type) = &func.meta().type_ {
                if arg_types.len() != values.len() {
                    todo!("error handling");
                }

                for (arg, type_) in values.iter_mut().zip(arg_types) {
                    substitute(&mut arg.meta_mut().type_, type_, substitutions, coercions)?;
                }

                substitute(&mut meta.type_, &**ret_type, substitutions, coercions)?;

                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::StructDef { .. } => Ok(()),

        SExpr::StructSet { meta, name, values } => {
            if let Some(struct_) = struct_map.get(name) {
                let mut map = HashMap::new();
                let mut generics = vec![];
                for g in struct_.generics.iter() {
                    if let &Type::Generic(g) = g {
                        map.insert(g, Type::TypeVariable(*type_var_counter));
                        generics.push(Type::TypeVariable(*type_var_counter));
                        *type_var_counter += 1;
                    } else {
                        unreachable!();
                    }
                }

                for (field, value) in values {
                    traverse_sexpr(value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
                    if let Some((_, typ)) = struct_.fields.iter().find(|(v, _)| v == field) {
                        let mut typ = typ.clone();
                        typ.replace_generics(type_var_counter, &mut map);
                        substitute(&mut value.meta_mut().type_, &typ, substitutions, coercions)?;
                    } else {
                        todo!("error handling");
                    }
                }

                substitute(&mut meta.type_, &Type::Struct(name, generics), substitutions, coercions)?;

                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::Declare { meta, variable, value, .. } => {
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            substitute(&mut meta.type_, &value.meta().type_, substitutions, coercions)?;
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
                substitute(&mut meta.type_, var_type, substitutions, coercions)?;
                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::Assign { meta, lvalue, value } => {
            let type_ = traverse_lvalue(lvalue, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            substitute(&mut meta.type_, &type_, substitutions, coercions)?;
            traverse_sexpr(&mut **value, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            substitute(&mut value.meta_mut().type_, &meta.type_, substitutions, coercions)
        }

        SExpr::Attribute { meta, top, attrs } => {
            traverse_sexpr(&mut **top, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;

            match &top.meta().type_ {
                Type::Slice(_, t) => {
                    if attrs.len() != 1 {
                        todo!("error handling");
                    }

                    match attrs[0] {
                        "ptr" => {
                            substitute(&mut meta.type_, &Type::Pointer(true, t.clone()), substitutions, coercions)
                        }

                        "len" | "cap" => {
                            substitute(&mut meta.type_, &Type::Int(false, 64), substitutions, coercions)
                        }

                        _ => todo!("error handling"),
                    }
                }

                Type::Struct(_, _) => {
                    let mut t = top.meta().type_.clone();
                    for &mut attr in attrs {
                        while let Type::TypeVariable(i) = t {
                            if let Some(u) = substitutions.get(&i) {
                                t = u.clone();
                            } else {
                                todo!("error handling");
                            }
                        }

                        match t {
                            Type::Slice(_, u) => {
                                match attr {
                                    "ptr" => {
                                        t = Type::Pointer(true, u.clone());
                                    }

                                    "len" | "cap" => {
                                        t = Type::Int(false, 64);
                                    }

                                    _ => todo!("error handling"),
                                }
                            }

                            Type::Struct(name, generics) => {
                                if let Some(struct_) = struct_map.get(name) {
                                    let mut map = struct_.generics.iter().zip(generics).map(|(key, val)| {
                                        if let &Type::Generic(key) = key {
                                            (key, val)
                                        } else {
                                            unreachable!();
                                        }
                                    }).collect();

                                    if let Some((_, typ)) = struct_.fields.iter().find(|(v, _)| *v == attr) {
                                        t = typ.clone();
                                        t.replace_generics(type_var_counter, &mut map);
                                    } else {
                                        todo!("error handling");
                                    }
                                } else {
                                    todo!("error handling");
                                }
                            }

                            _ => todo!("error handling"),
                        }
                    }

                    substitute(&mut meta.type_, &t, substitutions, coercions)
                }

                _ => todo!("error handling"),
            }
        }

        SExpr::SizeOf { .. } => Ok(()),
    }
}

#[allow(clippy::too_many_arguments)]
fn traverse_lvalue<'a>(
    lvalue: &mut LValue<'a>,
    type_var_counter: &mut u64,
    substitutions: &mut HashMap<u64, Type<'a>>,
    coercions: &mut HashMap<u64, FloatOrInt>,
    func_map: &HashMap<&'a str, Signature<'a>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
    monomorphisms: &mut Vec<(Type<'a>, usize)>,
    scopes: &mut Vec<HashMap<&'a str, Type<'a>>>,
    break_type: &mut Option<Type<'a>>,
) -> Result<Type<'a>, CorrectnessError> {
    match lvalue {
        LValue::Symbol(sym) => {
            for scope in scopes.iter().rev() {
                if let Some(typ) = scope.get(sym) {
                    return Ok(typ.clone());
                }
            }

            todo!("error handling");
        }

        LValue::Attribute(v, attrs) => {
            let mut t = traverse_lvalue(&mut **v, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            for &mut attr in attrs {
                while let Type::TypeVariable(i) = t {
                    if let Some(u) = substitutions.get(&i) {
                        t = u.clone();
                    } else {
                        todo!("error handling");
                    }
                }

                if let Type::Struct(name, generics) = t {
                    if let Some(struct_) = struct_map.get(name) {
                        let mut map = struct_.generics.iter().zip(generics).map(|(key, val)| {
                            if let &Type::Generic(key) = key {
                                (key, val)
                            } else {
                                unreachable!();
                            }
                        }).collect();

                        if let Some((_, typ)) = struct_.fields.iter().find(|(v, _)| *v == attr) {
                            t = typ.clone();
                            t.replace_generics(type_var_counter, &mut map);
                        } else {
                            todo!("error handling");
                        }
                    } else {
                        todo!("error handling");
                    }
                } else {
                    todo!("error handling");
                }
            }

            Ok(t)
        }

        LValue::Deref(v) => {
            let mut type_ = traverse_lvalue(&mut **v, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            match type_ {
                Type::Pointer(_, t) => Ok(*t),

                Type::TypeVariable(_) => {
                    let t = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    substitute(&mut type_, &Type::Pointer(true, Box::new(t.clone())), substitutions, coercions)?;
                    Ok(t)
                }

                _ => todo!("error handling"),
            }
        }

        LValue::Get(v, i) => {
            traverse_sexpr(&mut **i, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            substitute(&mut i.meta_mut().type_, &Type::Int(false, 64), substitutions, coercions)?;
            let mut type_ = traverse_lvalue(&mut **v, type_var_counter, substitutions, coercions, func_map, struct_map, monomorphisms, scopes, break_type)?;
            match type_ {
                Type::Slice(_, t) => Ok(*t),

                Type::TypeVariable(_) => {
                    let t = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    substitute(&mut type_, &Type::Slice(true, Box::new(t.clone())), substitutions, coercions)?;
                    Ok(t)
                }

                _ => todo!("error handling"),
            }
        }
    }
}

fn flatten_substitution<'a>(t1: &mut Type<'a>, substitutions: &HashMap<u64, Type<'a>>) {
    match t1 {
        Type::Tuple(v) => {
            for v in v {
                flatten_substitution(v, substitutions);
            }
        }

        Type::Pointer(_, v) => flatten_substitution(&mut **v, substitutions),
        Type::Slice(_, v) => flatten_substitution(&mut **v, substitutions),

        Type::Struct(_, v) => {
            for v in v {
                flatten_substitution(v, substitutions);
            }
        }

        Type::Function(a, r) => {
            for a in a {
                flatten_substitution(a, substitutions);
            }

            flatten_substitution(&mut **r, substitutions);
        }

        Type::TypeVariable(i) => {
            *t1 = substitutions.get(i).unwrap().clone();
            flatten_substitution(t1, substitutions);
        }

        _ => (),
    }
}

fn apply_lvalue_substitutions<'a>(lvalue: &mut LValue<'a>, substitutions: &HashMap<u64, Type<'a>>) {
    match lvalue {
        LValue::Symbol(_) => (),
        LValue::Attribute(v, _) => apply_lvalue_substitutions(&mut **v, substitutions),
        LValue::Deref(v) => apply_lvalue_substitutions(&mut **v, substitutions),
        LValue::Get(v, i) => {
            apply_lvalue_substitutions(&mut **v, substitutions);
            apply_substitutions(&mut **i, substitutions);
        }
    }
}

fn apply_substitutions<'a>(sexpr: &mut SExpr<'a>, substitutions: &HashMap<u64, Type<'a>>) {
    flatten_substitution(&mut sexpr.meta_mut().type_, substitutions);

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

        SExpr::Assign { lvalue, value, .. } => {
            apply_substitutions(&mut **value, substitutions);
            apply_lvalue_substitutions(lvalue, substitutions);
        }

        SExpr::Attribute { top, .. } => apply_substitutions(&mut **top, substitutions),

        SExpr::SizeOf { type_, .. } => flatten_substitution(type_, substitutions),

        _ => (),
    }
}

pub fn check<'a>(
    sexprs: &mut Vec<SExpr<'a>>,
    func_map: &HashMap<&'a str, Signature<'a>>,
    struct_map: &HashMap<&'a str, Struct<'a>>,
) -> Result<(), CorrectnessError> {
    let mut skip = 0;
    let mut done = HashSet::new();

    while {
        let mut monomorphisms = vec![];

        let mut type_var_counter = 0;
        let mut substitutions = HashMap::new();
        let mut coercions = HashMap::new();
        for sexpr in sexprs.iter_mut().skip(skip) {
            if let SExpr::FuncDef { ret_type, args, expr, .. } = sexpr {
                if args.iter().any(|(_, v)| v.has_generic()) || ret_type.has_generic() {
                    continue;
                }

                let mut scopes = vec![HashMap::new()];
                for (arg, type_) in args {
                    scopes.last_mut().unwrap().insert(*arg, type_.clone());
                }

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

                substitute(&mut expr.meta_mut().type_, ret_type, &mut substitutions, &mut coercions)?;

                for (&i, &coercion) in coercions.iter() {
                    match substitutions.entry(i) {
                        Entry::Occupied(v) => {
                            let mut t = v.get().clone();
                            while let Type::TypeVariable(j) = t {
                                if let Some(v) = substitutions.get(&j) {
                                    t = v.clone();
                                } else {
                                    match coercion {
                                        FloatOrInt::Float => t = Type::F64,
                                        FloatOrInt::Int => t = Type::Int(true, 32),
                                    };
                                    println!("uwu");
                                    substitutions.insert(j, t);
                                    break;
                                }
                            }
                        }

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

        skip = sexprs.len();

        if !monomorphisms.is_empty() {
            for (mut t, index) in monomorphisms {
                flatten_substitution(&mut t, &substitutions);

                if done.contains(&(t.clone(), index)) {
                    continue;
                }
                done.insert((t.clone(), index));

                let mut monomorphised = sexprs[index].clone();

                if let SExpr::FuncDef { meta, args, ret_type, expr, .. } = &mut monomorphised {
                    meta.type_ = t.clone();
                    let mut map = HashMap::new();
                    extract_generics(&meta.type_, &t, &mut map);
                    update_generic_refs(expr, &mut map);
                    if let Type::Function(a, r) = t {
                        for (i, (_, arg)) in args.iter_mut().enumerate() {
                            *arg = a[i].clone();
                        }

                        *ret_type = *r;

                        sexprs.push(monomorphised);
                    }
                }
            }

            skip != sexprs.len()
        } else {
            false
        }
    } {}

    Ok(())
}

fn update_generic_refs_lvalue<'a>(lvalue: &mut LValue<'a>, map: &mut HashMap<&'a str, Type<'a>>) {
    match lvalue {
        LValue::Symbol(_) => (),
        LValue::Attribute(v, _) => update_generic_refs_lvalue(&mut **v, map),
        LValue::Deref(v) => update_generic_refs_lvalue(&mut **v, map),
        LValue::Get(v, i) => {
            update_generic_refs_lvalue(&mut **v, map);
            update_generic_refs(&mut **i, map);
        }
    }
}

fn update_generic_refs<'a>(expr: &mut SExpr<'a>, map: &mut HashMap<&'a str, Type<'a>>) {
    match expr {
        SExpr::List { .. } => todo!(),
        SExpr::Quote { .. } => todo!(),
        SExpr::Comma { .. } => todo!(),
        SExpr::Backtick { .. } => todo!(),
        SExpr::Splice { .. } => todo!(),

        SExpr::Seq { values, .. } => {
            for value in values {
                update_generic_refs(value, map);
            }
        }

        SExpr::Cond { values, elsy, .. } => {
            for (cond, then) in values {
                update_generic_refs(cond, map);
                update_generic_refs(then, map);
            }

            if let Some(elsy) = elsy {
                update_generic_refs(&mut **elsy, map);
            }
        }

        SExpr::Loop { value, .. } => update_generic_refs(&mut **value, map),

        SExpr::Break { value: Some(value), .. } => update_generic_refs(&mut **value, map),
        SExpr::Type { value, .. } => update_generic_refs(&mut **value, map),
        SExpr::FuncCall { func, values, .. } => {
            update_generic_refs(&mut **func, map);
            for value in values {
                update_generic_refs(value, map);
            }
        }
        SExpr::StructSet { values, .. } => {
            for (_, value) in values {
                update_generic_refs(value, map);
            }
        }
        SExpr::Declare { value, .. } => update_generic_refs(&mut **value, map),
        SExpr::Assign { lvalue, value, .. } => {
            update_generic_refs_lvalue(lvalue, map);
            update_generic_refs(&mut **value, map);
        }
        SExpr::Attribute { top, .. } => update_generic_refs(&mut **top, map),
        SExpr::SizeOf { type_, .. } => {
            let mut tvc = 0;
            type_.replace_generics(&mut tvc, map);
        }

        _ => (),
    }
}

fn extract_generics<'a>(original: &Type<'a>, monomorphised: &Type<'a>, map: &mut HashMap<&'a str, Type<'a>>) {
    match (original, monomorphised) {
        (Type::Tuple(v1), Type::Tuple(v2)) => {
            for (v1, v2) in v1.iter().zip(v2) {
                extract_generics(v1, v2, map);
            }
        }
        (Type::Pointer(_, v1), Type::Pointer(_, v2)) => extract_generics(&**v1, &**v2, map),
        (Type::Slice(_, v1), Type::Slice(_, v2)) => extract_generics(&**v1, &**v2, map),

        (Type::Struct(_, v1), Type::Struct(_, v2)) => {
            for (v1, v2) in v1.iter().zip(v2) {
                extract_generics(v1, v2, map);
            }
        }

        (Type::Generic(g), v) => {
            map.insert(*g, v.clone());
        }

        (Type::Function(a1, r1), Type::Function(a2, r2)) => {
            for (a1, a2) in a1.iter().zip(a2) {
                extract_generics(a1, a2, map);
            }

            extract_generics(&**r1, &**r2, map);
        }

        _ => (),
    }
}

#[derive(Debug, Clone)]
pub struct Signature<'a> {
    pub arg_types: Vec<Type<'a>>,
    pub ret_type: Type<'a>,
    pub index: Option<usize>,
}

pub fn create_default_signatures<'a>() -> HashMap<&'a str, Signature<'a>> {
    let mut map = HashMap::new();
    map.insert("+", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("-", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("*", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("/", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("%", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("<", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert(">", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert("<=", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert(">=", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert("<<", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert(">>", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("&", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("|", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("^", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Generic("a"),
        index: None,
    });
    map.insert("==", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert("!=", Signature {
        arg_types: vec![Type::Generic("a"), Type::Generic("a")],
        ret_type: Type::Int(false, 1),
        index: None,
    });
    map.insert("cast", Signature {
        arg_types: vec![Type::Generic("a")],
        ret_type: Type::Generic("b"),
        index: None,
    });
    map.insert(
        "alloca",
        Signature {
            arg_types: vec![Type::Int(false, 64)],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    );
    map.insert(
        "ref",
        Signature {
            arg_types: vec![Type::Generic("a")],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    );
    map.insert(
        "deref",
        Signature {
            arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a")))],
            ret_type: Type::Generic("a"),
            index: None,
        },
    );
    map.insert(
        "get",
        Signature {
            arg_types: vec![
                Type::Slice(true, Box::new(Type::Generic("a"))),
                Type::Int(false, 64),
            ],
            ret_type: Type::Generic("a"),
            index: None,
        },
    );
    map.insert("ptr-add", Signature {
        arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 64)],
        ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
        index: None,
    });
    map.insert("ptr-sub", Signature {
        arg_types: vec![Type::Pointer(true, Box::new(Type::Generic("a"))), Type::Int(false, 64)],
        ret_type: Type::Pointer(true, Box::new(Type::Generic("a"))),
        index: None,
    });
    map.insert(
        "slice",
        Signature {
            arg_types: vec![
                Type::Int(false, 64),
                Type::Pointer(true, Box::new(Type::Generic("a"))),
            ],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a"))),
            index: None,
        },
    );
    map.insert(
        "syscall",
        Signature {
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
        },
    );

    map
}

pub fn extract_signatures<'a>(
    sexprs: &[SExpr<'a>],
    map: &mut HashMap<&'a str, Signature<'a>>,
) {
    for (i, sexpr) in sexprs.iter().enumerate() {
        if let SExpr::FuncDef { meta, name, .. } = sexpr {
            let (arg_types, ret_type) = match &meta.type_ {
                Type::Function(a, r) => (a.clone(), (**r).clone()),
                _ => unreachable!(),
            };
            let index = Some(i);

            match map.entry(*name) {
                Entry::Occupied(_) => {
                    todo!("error handling");
                }

                Entry::Vacant(entry) => {
                    entry.insert(Signature {
                        arg_types,
                        ret_type,
                        index,
                    });
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
