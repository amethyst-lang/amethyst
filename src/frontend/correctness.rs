use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    path::{Path, PathBuf},
};

use super::ast_lowering::{Annotations, SExpr, Type};

#[derive(Debug)]
pub enum CorrectnessError {}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum FloatOrInt {
    Float,
    Int,
}

fn get_leaf<'a, 'b>(mut t: &'b Type, substitutions: &'b HashMap<u64, Type>) -> &'b Type {
    while let Type::TypeVariable(i) = t {
        if let Some(u) = substitutions.get(i) {
            t = u;
        } else {
            break;
        }
    }
    t
}

fn check_coercion(coercions: &mut HashMap<u64, FloatOrInt>, i: &u64, t: &Type) -> bool {
    match (coercions.get(i), &t) {
        (Some(FloatOrInt::Int), Type::Int(_, _)) => true,
        (Some(FloatOrInt::Float), Type::F32 | Type::F64) => true,
        (Some(&v), Type::TypeVariable(j)) => match coercions.entry(*j) {
            Entry::Occupied(w) => *w.get() == v,

            Entry::Vacant(w) => {
                w.insert(v);
                true
            }
        },

        (None, _) => true,

        (Some(_), Type::Union(v)) => v.iter().any(|v| check_coercion(coercions, i, v)),

        _ => false,
    }
}

fn substitute(
    assignee: &mut Type,
    assigner: &Type,
    substitutions: &mut HashMap<u64, Type>,
    coercions: &mut HashMap<u64, FloatOrInt>,
) -> Result<(), CorrectnessError> {
    *assignee = get_leaf(assignee, substitutions).clone();
    let assigner = get_leaf(assigner, substitutions).clone();

    if assignee.is_subtype(&assigner) {
        *assignee = assigner;
        Ok(())
    } else if *assignee == Type::Unknown {
        *assignee = assigner.clone();
        Ok(())
    } else if let Type::TypeVariable(i) = assignee {
        if let Some(t) = substitutions.insert(*i, assigner.clone()) {
            if !assigner.is_subtype(&t) {
                todo!("error handling");
            } else {
                *assignee = assigner;
                Ok(())
            }
        } else {
            *assignee = assigner;
            Ok(())
        }
    } else if let Type::TypeVariable(i) = assigner {
        if !check_coercion(coercions, &i, assignee) {
            todo!("error handling");
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

            (Type::Pointer(_, a), Type::Pointer(_, b)) => {
                substitute(&mut **a, &*b, substitutions, coercions)
            }
            (Type::Slice(_, a), Type::Slice(_, b)) => {
                substitute(&mut **a, &*b, substitutions, coercions)
            }

            (Type::Struct(n1, v1), Type::Struct(n2, v2)) if *n1 == n2 => {
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
            (a, b) => todo!("error handling: {:?} vs {:?}", a, b),
        }
    }
}

#[derive(Copy, Clone)]
enum LetStatus {
    NoLet,
    Conditional,
    Direct,
}

#[allow(clippy::too_many_arguments)]
fn traverse_sexpr(
    sexpr: &mut SExpr,
    type_var_counter: &mut u64,
    substitutions: &mut HashMap<u64, Type>,
    coercions: &mut HashMap<u64, FloatOrInt>,
    func_map: &mut Vec<HashMap<String, Signature>>,
    struct_map: &mut Vec<HashMap<String, Struct>>,
    monomorphisms: &mut Vec<(Type, usize)>,
    scopes: &mut Vec<HashMap<String, (Type, LetStatus)>>,
    break_type: &mut Option<Type>,
    let_status: LetStatus,
    exports: &HashMap<PathBuf, ModuleExports>,
) -> Result<(), CorrectnessError> {
    //println!("{:?}: {:?}\n", sexpr.meta().range, sexpr);

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
        SExpr::Int { .. } | SExpr::Float { .. } | SExpr::Str { .. } | SExpr::Nil { .. } => Ok(()),

        SExpr::Symbol { meta, value } => {
            let mut var = None;
            for scope in scopes.iter().rev() {
                if let Some(t) = scope.get(value) {
                    var = Some(t);
                    break;
                }
            }

            if let Some((var_type, let_status)) = var {
                if let LetStatus::Conditional = let_status {
                    todo!("error handling");
                }

                substitute(&mut meta.type_, var_type, substitutions, coercions)?;
                Ok(())
            } else if let Some(func) = {
                let mut var = None;
                for scope in func_map.iter().rev() {
                    if let Some(v) = scope.get(value) {
                        var = Some(v);
                        break;
                    }
                }
                var
            } {
                substitute(
                    &mut meta.type_,
                    &Type::Function(func.arg_types.clone(), Box::new(func.ret_type.clone())),
                    substitutions,
                    coercions,
                )?;
                if meta.type_.has_generic() {
                    let mut map = HashMap::new();
                    meta.type_.replace_generics(type_var_counter, &mut map);
                    if let Some(index) = func.index {
                        monomorphisms.push((meta.type_.clone(), index));
                    }
                }
                Ok(())
            } else {
                todo!("error handling: {}", value);
            }
        }

        SExpr::Tuple { meta, tuple } => {
            for value in tuple.iter_mut() {
                traverse_sexpr(
                    value,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;
            }

            let new_type = Type::Tuple(
                tuple
                    .iter()
                    .map(|_| {
                        let t = Type::TypeVariable(*type_var_counter);
                        *type_var_counter += 1;
                        t
                    })
                    .collect(),
            );
            substitute(&mut meta.type_, &new_type, substitutions, coercions)
        }

        SExpr::Seq { meta, values } => {
            if !matches!(let_status, LetStatus::Conditional) {
                scopes.push(HashMap::new());
            }
            func_map.push(HashMap::new());
            struct_map.push(HashMap::new());
            for value in values.iter_mut() {
                traverse_sexpr(
                    value,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;
            }

            if let Some(last) = values.last() {
                substitute(
                    &mut meta.type_,
                    &last.meta().type_,
                    substitutions,
                    coercions,
                )?;
            }

            if !matches!(let_status, LetStatus::Conditional) {
                scopes.pop();
            }
            func_map.pop();
            struct_map.pop();
            Ok(())
        }

        SExpr::Cond { meta, values, elsy } => {
            for (cond, then) in values.iter_mut() {
                if !matches!(let_status, LetStatus::Conditional) {
                    scopes.push(HashMap::new());
                }
                func_map.push(HashMap::new());
                struct_map.push(HashMap::new());
                traverse_sexpr(
                    cond,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;

                for (_, let_status) in scopes.last_mut().unwrap().values_mut() {
                    *let_status = LetStatus::Direct;
                }

                traverse_sexpr(
                    then,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;

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

                substitute(
                    &mut meta.type_,
                    &then.meta().type_,
                    substitutions,
                    coercions,
                )?;

                if !matches!(let_status, LetStatus::Conditional) {
                    scopes.pop();
                }
                func_map.pop();
                struct_map.pop();
            }

            if let Some(elsy) = elsy {
                if !matches!(let_status, LetStatus::Conditional) {
                    scopes.push(HashMap::new());
                }
                func_map.push(HashMap::new());
                struct_map.push(HashMap::new());
                traverse_sexpr(
                    &mut **elsy,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;
                substitute(
                    &mut meta.type_,
                    &elsy.meta().type_,
                    substitutions,
                    coercions,
                )?;
                if !matches!(let_status, LetStatus::Conditional) {
                    scopes.pop();
                }
                func_map.pop();
                struct_map.pop();
            } else if meta.type_ != Type::Tuple(vec![]) {
                todo!("error handling");
            }

            Ok(())
        }

        SExpr::Loop { meta, value } => {
            if !matches!(let_status, LetStatus::Conditional) {
                scopes.push(HashMap::new());
            }
            func_map.push(HashMap::new());
            struct_map.push(HashMap::new());
            let mut break_type = Some(Type::Unknown);
            traverse_sexpr(
                &mut **value,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                &mut break_type,
                let_status,
                exports,
            )?;

            let t = match break_type {
                Some(Type::Unknown) => Type::Tuple(vec![]),
                Some(v) => v,
                None => unreachable!(),
            };
            substitute(&mut meta.type_, &t, substitutions, coercions)?;
            if !matches!(let_status, LetStatus::Conditional) {
                scopes.pop();
            }
            func_map.pop();
            struct_map.pop();

            Ok(())
        }

        SExpr::Break { value, .. } => {
            if let Some(value) = value {
                traverse_sexpr(
                    &mut **value,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;
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
            traverse_sexpr(
                &mut **value,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;
            substitute(
                &mut value.meta_mut().type_,
                &meta.type_,
                substitutions,
                coercions,
            )
        }

        SExpr::FuncDef { .. } => Ok(()),

        SExpr::FuncCall { meta, func, values } => {
            traverse_sexpr(
                &mut **func,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;

            for value in values.iter_mut() {
                traverse_sexpr(
                    value,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    let_status,
                    exports,
                )?;
            }

            if let Type::Function(arg_types, ret_type) = &func.meta().type_ {
                if arg_types.len() != values.len() {
                    todo!("error handling {:?}", func);
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

        SExpr::FuncExtern { .. } => Ok(()),

        SExpr::StructDef { .. } => Ok(()),

        SExpr::StructSet { meta, name, values } => {
            if let Some(struct_) = {
                let mut var = None;
                for scope in struct_map.iter().rev() {
                    if let Some(v) = scope.get(name) {
                        var = Some(v);
                        break;
                    }
                }
                var
            } {
                let struct_ = struct_.clone();
                let mut map = HashMap::new();
                let mut generics = vec![];
                for g in struct_.generics.iter() {
                    if let Type::Generic(g) = g {
                        map.insert(g.clone(), Type::TypeVariable(*type_var_counter));
                        generics.push(Type::TypeVariable(*type_var_counter));
                        *type_var_counter += 1;
                    } else {
                        unreachable!();
                    }
                }

                for (field, value) in values {
                    traverse_sexpr(
                        value,
                        type_var_counter,
                        substitutions,
                        coercions,
                        func_map,
                        struct_map,
                        monomorphisms,
                        scopes,
                        break_type,
                        let_status,
                        exports,
                    )?;
                    if let Some((_, typ)) = struct_.fields.iter().find(|(v, _)| v == field) {
                        let mut typ = typ.clone();
                        typ.replace_generics(type_var_counter, &mut map);
                        substitute(&mut value.meta_mut().type_, &typ, substitutions, coercions)?;
                    } else {
                        todo!("error handling for {}", field);
                    }
                }

                substitute(
                    &mut meta.type_,
                    &Type::Struct(name.clone(), generics),
                    substitutions,
                    coercions,
                )?;

                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::Declare {
            meta,
            conditional,
            settings,
            ..
        } => {
            substitute(
                &mut meta.type_,
                &Type::Int(false, 1),
                substitutions,
                coercions,
            )?;

            for setting in settings {
                traverse_sexpr(
                    setting,
                    type_var_counter,
                    substitutions,
                    coercions,
                    func_map,
                    struct_map,
                    monomorphisms,
                    scopes,
                    break_type,
                    if *conditional {
                        LetStatus::Conditional
                    } else {
                        LetStatus::Direct
                    },
                    exports,
                )?;
                substitute(
                    &mut meta.type_,
                    &setting.meta().type_,
                    substitutions,
                    coercions,
                )?;
            }
            Ok(())
        }

        SExpr::Assign { meta, var, value } => {
            traverse_sexpr(
                &mut **value,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;
            substitute(
                &mut meta.type_,
                &Type::Int(false, 1),
                substitutions,
                coercions,
            )?;

            let mut var_type = None;
            for scope in scopes.iter_mut().rev() {
                if let Some(t) = scope.get_mut(var) {
                    var_type = Some(t);
                    break;
                }
            }

            if let Some((var_type, _)) = var_type {
                substitute(var_type, &value.meta().type_, substitutions, coercions)?;
                Ok(())
            } else if let LetStatus::Direct | LetStatus::Conditional = let_status {
                scopes
                    .last_mut()
                    .unwrap()
                    .insert(var.clone(), (value.meta().type_.clone(), let_status));
                Ok(())
            } else {
                todo!("error handling");
            }
        }

        SExpr::Attribute { meta, top, attr } => {
            traverse_sexpr(
                &mut **top,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;

            match &top.meta().type_ {
                Type::Slice(_, t) => match attr.as_str() {
                    "ptr" => substitute(
                        &mut meta.type_,
                        &Type::Pointer(true, t.clone()),
                        substitutions,
                        coercions,
                    ),

                    "len" | "cap" => substitute(
                        &mut meta.type_,
                        &Type::Int(false, 64),
                        substitutions,
                        coercions,
                    ),

                    _ => todo!("error handling"),
                },

                Type::Struct(_, _) => {
                    let mut t = top.meta().type_.clone();
                    while let Type::TypeVariable(i) = t {
                        if let Some(u) = substitutions.get(&i) {
                            t = u.clone();
                        } else {
                            todo!("error handling");
                        }
                    }

                    match t {
                        Type::Slice(_, u) => match attr.as_str() {
                            "ptr" => {
                                t = Type::Pointer(true, u.clone());
                            }

                            "len" | "cap" => {
                                t = Type::Int(false, 64);
                            }

                            _ => todo!("error handling"),
                        },

                        Type::Struct(name, generics) => {
                            if let Some(struct_) = {
                                let mut var = None;
                                for scope in struct_map.iter().rev() {
                                    if let Some(v) = scope.get(&name) {
                                        var = Some(v);
                                        break;
                                    }
                                }
                                var
                            } {
                                let mut map = struct_
                                    .generics
                                    .iter()
                                    .zip(generics)
                                    .map(|(key, val)| {
                                        if let Type::Generic(key) = key {
                                            (key.clone(), val)
                                        } else {
                                            unreachable!();
                                        }
                                    })
                                    .collect();

                                if let Some((_, typ)) =
                                    struct_.fields.iter().find(|(v, _)| v == attr)
                                {
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

                    substitute(&mut meta.type_, &t, substitutions, coercions)
                }

                _ => todo!("error handling"),
            }
        }

        SExpr::SliceGet { meta, top, index } => {
            traverse_sexpr(
                &mut **top,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;

            traverse_sexpr(
                &mut **index,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;

            substitute(
                &mut index.meta_mut().type_,
                &Type::Int(false, 64),
                substitutions,
                coercions,
            )?;
            match &top.meta().type_ {
                Type::Slice(_, t) => substitute(&mut meta.type_, &**t, substitutions, coercions),
                t @ Type::TypeVariable(_) => substitute(
                    &mut Type::Slice(false, Box::new(meta.type_.clone())),
                    t,
                    substitutions,
                    coercions,
                ),

                _ => todo!("error handling"),
            }
        }

        SExpr::SizeOf { .. } => Ok(()),

        SExpr::Ref { meta, value } => {
            traverse_sexpr(
                value,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;
            let t = Type::Pointer(false, Box::new(value.meta().type_.clone()));
            substitute(&mut meta.type_, &t, substitutions, coercions)
        }

        SExpr::Deref { meta, value } => {
            traverse_sexpr(
                value,
                type_var_counter,
                substitutions,
                coercions,
                func_map,
                struct_map,
                monomorphisms,
                scopes,
                break_type,
                let_status,
                exports,
            )?;
            let t = Type::Pointer(false, Box::new(meta.type_.clone()));
            substitute(&mut value.meta_mut().type_, &t, substitutions, coercions)
        }

        SExpr::Import { imports, .. } => {
            for (named_as, module_path) in imports {
                match named_as {
                    Some(_) => todo!(),

                    None => {
                        if let Some(exported) =
                            exports.get(&Path::new(module_path).canonicalize().unwrap())
                        {
                            for func in exported.function_exports.iter() {
                                func_map.last_mut().unwrap().insert(
                                    func.clone(),
                                    exported.func_map.get(func).unwrap().clone(),
                                );
                            }

                            for struct_ in exported.struct_exports.iter() {
                                struct_map.last_mut().unwrap().insert(
                                    struct_.clone(),
                                    exported.struct_map.get(struct_).unwrap().clone(),
                                );
                            }
                        } else {
                            todo!("error handling: unknown module \"{}\"", module_path);
                        }
                    }
                }
            }

            Ok(())
        }
    }
}

fn flatten_substitution(t1: &mut Type, substitutions: &HashMap<u64, Type>) {
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

fn apply_substitutions(sexpr: &mut SExpr, substitutions: &HashMap<u64, Type>) {
    flatten_substitution(&mut sexpr.meta_mut().type_, substitutions);

    match sexpr {
        SExpr::Tuple { tuple, .. } => {
            for value in tuple {
                apply_substitutions(value, substitutions);
            }
        }

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
        SExpr::Break {
            value: Some(value), ..
        } => apply_substitutions(&mut **value, substitutions),

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

        SExpr::Declare { settings, .. } => {
            for setting in settings {
                apply_substitutions(setting, substitutions);
            }
        }

        SExpr::Assign { value, .. } => {
            apply_substitutions(&mut **value, substitutions);
        }

        SExpr::Attribute { top, .. } => apply_substitutions(&mut **top, substitutions),
        SExpr::SliceGet { top, index, .. } => {
            apply_substitutions(&mut **top, substitutions);
            apply_substitutions(&mut **index, substitutions);
        }

        SExpr::SizeOf { type_, .. } => flatten_substitution(type_, substitutions),

        SExpr::Ref { value, .. } => apply_substitutions(&mut **value, substitutions),
        SExpr::Deref { value, .. } => apply_substitutions(&mut **value, substitutions),

        _ => (),
    }
}

pub fn check(
    name: &PathBuf,
    module: &mut Module,
    exports: &HashMap<PathBuf, ModuleExports>,
) -> Result<(), CorrectnessError> {
    let mut skip = 0;
    let mut done = HashSet::new();

    let this_module = exports.get(name).unwrap();
    let mut func_map = vec![this_module.func_map.clone()];
    let mut struct_map = vec![this_module.struct_map.clone()];

    while {
        let mut monomorphisms = vec![];

        let mut type_var_counter = 0;
        let mut substitutions = HashMap::new();
        let mut coercions = HashMap::new();
        for sexpr in module.sexprs.iter_mut().skip(skip) {
            match sexpr {
                SExpr::FuncDef {
                    ret_type,
                    args,
                    expr,
                    ..
                } => {
                    if args.iter().any(|(_, v)| v.has_generic()) || ret_type.has_generic() {
                        continue;
                    }

                    let mut scopes = vec![HashMap::new()];
                    for (arg, type_) in args {
                        scopes
                            .last_mut()
                            .unwrap()
                            .insert(arg.to_string(), (type_.clone(), LetStatus::Direct));
                    }

                    traverse_sexpr(
                        expr,
                        &mut type_var_counter,
                        &mut substitutions,
                        &mut coercions,
                        &mut func_map,
                        &mut struct_map,
                        &mut monomorphisms,
                        &mut scopes,
                        &mut None,
                        LetStatus::NoLet,
                        exports,
                    )?;

                    substitute(
                        &mut expr.meta_mut().type_,
                        ret_type,
                        &mut substitutions,
                        &mut coercions,
                    )?;

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

                SExpr::Import { .. } => {
                    traverse_sexpr(
                        sexpr,
                        &mut type_var_counter,
                        &mut substitutions,
                        &mut coercions,
                        &mut func_map,
                        &mut struct_map,
                        &mut monomorphisms,
                        &mut Vec::new(),
                        &mut None,
                        LetStatus::NoLet,
                        exports,
                    )?;
                }

                _ => (),
            }
        }

        skip = module.sexprs.len();

        if !monomorphisms.is_empty() {
            for (mut t, index) in monomorphisms {
                flatten_substitution(&mut t, &substitutions);

                if done.contains(&(t.clone(), index)) {
                    continue;
                }
                done.insert((t.clone(), index));

                let mut monomorphised = module.sexprs[index].clone();

                if let SExpr::FuncDef {
                    meta,
                    args,
                    ret_type,
                    expr,
                    ..
                } = &mut monomorphised
                {
                    let mut map = HashMap::new();
                    extract_generics(&meta.type_, &t, &mut map);
                    update_generic_refs(expr, &mut map);
                    meta.type_ = t.clone();
                    if let Type::Function(a, r) = t {
                        for (i, (_, arg)) in args.iter_mut().enumerate() {
                            *arg = a[i].clone();
                        }

                        *ret_type = *r;

                        module.sexprs.push(monomorphised);
                    }
                }
            }

            skip != module.sexprs.len()
        } else {
            false
        }
    } {}

    Ok(())
}

fn update_generic_refs(expr: &mut SExpr, map: &mut HashMap<String, Type>) {
    match expr {
        SExpr::Tuple { tuple, .. } => {
            for value in tuple {
                update_generic_refs(value, map);
            }
        }

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

        SExpr::Break {
            value: Some(value), ..
        } => update_generic_refs(&mut **value, map),
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
        SExpr::Declare { settings, .. } => {
            for setting in settings {
                update_generic_refs(setting, map);
            }
        }
        SExpr::Assign { value, .. } => {
            update_generic_refs(&mut **value, map);
        }
        SExpr::Attribute { top, .. } => update_generic_refs(&mut **top, map),
        SExpr::SliceGet { top, index, .. } => {
            update_generic_refs(&mut **top, map);
            update_generic_refs(&mut **index, map);
        }
        SExpr::SizeOf { type_, .. } => {
            let mut tvc = 0;
            type_.replace_generics(&mut tvc, map);
        }

        SExpr::Ref { value, .. } => update_generic_refs(&mut **value, map),
        SExpr::Deref { value, .. } => update_generic_refs(&mut **value, map),

        _ => (),
    }
}

fn extract_generics(original: &Type, monomorphised: &Type, map: &mut HashMap<String, Type>) {
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
            map.insert(g.clone(), v.clone());
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
pub enum SignatureLinkage {
    Builtin,
    External,
    Defined,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub index: Option<usize>,
    pub linkage: SignatureLinkage,
}

// TODO: replace with real stuff
fn create_default_signatures() -> HashMap<String, Signature> {
    let mut map = HashMap::new();
    map.insert(
        "+".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "-".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "*".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "/".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "%".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "<".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        ">".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "<=".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        ">=".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "<<".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        ">>".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "&".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "|".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "^".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Generic("a".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "==".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "!=".to_string(),
        Signature {
            arg_types: vec![
                Type::Generic("a".to_string()),
                Type::Generic("a".to_string()),
            ],
            ret_type: Type::Int(false, 1),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "cast".to_string(),
        Signature {
            arg_types: vec![Type::Generic("a".to_string())],
            ret_type: Type::Generic("b".to_string()),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "alloca".to_string(),
        Signature {
            arg_types: vec![Type::Int(false, 64)],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a".to_string()))),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "ptr-add".to_string(),
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a".to_string()))),
                Type::Int(false, 64),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a".to_string()))),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "ptr-sub".to_string(),
        Signature {
            arg_types: vec![
                Type::Pointer(true, Box::new(Type::Generic("a".to_string()))),
                Type::Int(false, 64),
            ],
            ret_type: Type::Pointer(true, Box::new(Type::Generic("a".to_string()))),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map.insert(
        "slice".to_string(),
        Signature {
            arg_types: vec![
                Type::Int(false, 64),
                Type::Pointer(true, Box::new(Type::Generic("a".to_string()))),
            ],
            ret_type: Type::Slice(true, Box::new(Type::Generic("a".to_string()))),
            index: None,
            linkage: SignatureLinkage::Builtin,
        },
    );
    map
}

fn extract_signatures(sexprs: &[SExpr], map: &mut HashMap<String, Signature>) {
    for (i, sexpr) in sexprs.iter().enumerate() {
        if let SExpr::FuncDef { meta, name, .. } | SExpr::FuncExtern { meta, name, .. } = sexpr {
            let (arg_types, ret_type) = match &meta.type_ {
                Type::Function(a, r) => (a.clone(), (**r).clone()),
                _ => unreachable!(),
            };
            let index = Some(i);

            match map.entry(name.to_string()) {
                Entry::Occupied(_) => {
                    todo!("error handling");
                }

                Entry::Vacant(entry) => {
                    entry.insert(Signature {
                        arg_types,
                        ret_type,
                        index,
                        linkage: if let SExpr::FuncExtern { .. } = sexpr {
                            SignatureLinkage::External
                        } else {
                            SignatureLinkage::Defined
                        },
                    });
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<Type>,
    pub fields: Vec<(String, Type)>,
}

fn extract_structs(sexprs: &[SExpr], map: &mut HashMap<String, Struct>) {
    for sexpr in sexprs.iter() {
        if let SExpr::StructDef { name, fields, .. } = sexpr {
            let mut generics = vec![];

            for (_, t) in fields {
                t.find_generics(&mut generics);
            }

            let struct_ = Struct {
                name: name.clone(),
                generics,
                fields: fields.clone(),
            };

            if map.insert(name.clone(), struct_).is_some() {
                todo!("error handling");
            }
        }
    }
}

pub fn create_module(sexprs: Vec<SExpr>) -> (Module, ModuleExports) {
    let mut func_map = create_default_signatures();
    extract_signatures(&sexprs, &mut func_map);
    let mut struct_map = HashMap::new();
    extract_structs(&sexprs, &mut struct_map);

    let mut function_exports = Vec::new();
    let mut struct_exports = Vec::new();

    for sexpr in sexprs.iter() {
        match sexpr {
            SExpr::FuncDef {
                ann: Annotations { public: true, .. },
                name,
                ..
            } => {
                function_exports.push(name.clone());
            }

            SExpr::StructDef {
                ann: Annotations { public: true, .. },
                name,
                ..
            } => {
                struct_exports.push(name.clone());
            }

            _ => (),
        }
    }

    (
        Module { sexprs },
        ModuleExports {
            func_map,
            function_exports,
            struct_map,
            struct_exports,
        },
    )
}

pub struct Module {
    pub sexprs: Vec<SExpr>,
}

#[derive(Debug)]
pub struct ModuleExports {
    func_map: HashMap<String, Signature>,
    function_exports: Vec<String>,
    struct_map: HashMap<String, Struct>,
    struct_exports: Vec<String>,
}
