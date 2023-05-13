use std::collections::{hash_map::Entry, HashMap, HashSet};

use crate::{parse::{Ast, BaseType, BinaryOp, Pattern, Type}, lexer::Span};

#[derive(Debug)]
pub struct CheckError {
    pub message: String,
    pub primary_label: String,
    pub primary_label_loc: Span,
    pub secondary_labels: Vec<(String, Span)>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone)]
#[allow(unused)]
struct Class {
    generics: Vec<String>,
    constraints: Vec<(String, Vec<Type>)>, // TODO
    funcs: HashMap<String, Type>,
}

#[derive(Debug, Default)]
struct Environment {
    variables: Vec<(String, Type, Vec<(String, Vec<Type>)>)>,
    constructors: HashMap<String, (Vec<Type>, Type)>,
    substitutions: Vec<Type>,
    classes: HashMap<String, Class>,
    instances: Vec<(String, Vec<Type>, Vec<(String, Vec<Type>)>)>,
    constraints_applied: Vec<(String, Vec<Type>)>,
}

impl Environment {
    fn new_type_var(&mut self) -> Type {
        let t = Type::TypeVar(self.substitutions.len());
        self.substitutions.push(t.clone());
        t
    }

    fn push_variable(&mut self, var: &str, type_: &Type, constraints: &[(String, Vec<Type>)]) {
        self.variables
            .push((var.to_string(), type_.clone(), constraints.to_vec()));
    }

    fn pop_variable(&mut self) {
        self.variables.pop();
    }

    fn find_variable(&mut self, var: &str) -> Option<&(String, Type, Vec<(String, Vec<Type>)>)> {
        self.variables.iter().rev().find(|(v, ..)| v == var)
    }

    fn update_vars(&mut self) {
        let mut temp = Vec::new();
        std::mem::swap(&mut temp, &mut self.variables);
        for (_, var, _) in temp.iter_mut() {
            var.replace_type_vars(self);
        }
        std::mem::swap(&mut temp, &mut self.variables);

        let mut temp = HashMap::new();
        std::mem::swap(&mut temp, &mut self.classes);
        for (_, class) in temp.iter_mut() {
            for (_, func) in class.funcs.iter_mut() {
                func.replace_type_vars(self);
            }
        }
        std::mem::swap(&mut temp, &mut self.classes);
    }

    fn check_constraints(&mut self, _errors: &mut Vec<CheckError>) {
        let mut instances = self.instances.clone();

        while {
            let mut constraints_applied = Vec::new();
            std::mem::swap(&mut constraints_applied, &mut self.constraints_applied);

            'a: for (name, params) in constraints_applied.iter_mut() {
                if let Some(class) = self.classes.get(name) {
                    if class.generics.len() != params.len() {
                        todo!()
                    }
                } else {
                    todo!()
                }

                for p in params.iter_mut() {
                    p.replace_type_vars(self);
                }

                if params.iter().all(|t| matches!(t, Type::TypeVar(_))) {
                    continue 'a;
                }

                for (n, p, c) in instances.iter_mut() {
                    if n != name || p.len() != params.len() {
                        continue;
                    }

                    let mut same = true;
                    let mut generics = HashMap::new();
                    // TODO: so much cloning,,, fix that
                    for (a, mut b) in params.iter_mut().zip(p.iter().cloned()) {
                        b.convert_generics_to_type_vars(self, &mut generics);
                        if !a.equals_up_to_env(&mut b, self) {
                            same = false;
                            break;
                        }
                    }

                    if same {
                        for (_, c) in c.iter_mut() {
                            for c in c {
                                c.convert_generics_to_type_vars(self, &mut generics);
                            }
                        }

                        self.constraints_applied.extend(c.iter().cloned());
                        continue 'a;
                    }
                }

                todo!()
            }

            !self.constraints_applied.is_empty()
        } {}
    }
}

impl Type {
    fn equals_up_to_env(&mut self, other: &mut Type, env: &mut Environment) -> bool {
        match (self, other) {
            (Type::Base(BaseType::Bottom), _) | (_, Type::Base(BaseType::Bottom)) => true,
            (Type::Base(BaseType::Bool), Type::Base(BaseType::Bool)) => true,
            (Type::Base(BaseType::I8), Type::Base(BaseType::I8)) => true,
            (Type::Base(BaseType::I16), Type::Base(BaseType::I16)) => true,
            (Type::Base(BaseType::I32), Type::Base(BaseType::I32)) => true,
            (Type::Base(BaseType::I64), Type::Base(BaseType::I64)) => true,
            (Type::Base(BaseType::U8), Type::Base(BaseType::U8)) => true,
            (Type::Base(BaseType::U16), Type::Base(BaseType::U16)) => true,
            (Type::Base(BaseType::U32), Type::Base(BaseType::U32)) => true,
            (Type::Base(BaseType::U64), Type::Base(BaseType::U64)) => true,
            (Type::Base(BaseType::F32), Type::Base(BaseType::F32)) => true,
            (Type::Base(BaseType::F64), Type::Base(BaseType::F64)) => true,
            (Type::Base(BaseType::Named(n1, p1, _)), Type::Base(BaseType::Named(n2, p2, _))) => {
                n1 == n2
                    && p1.len() == p2.len()
                    && p1
                        .iter_mut()
                        .zip(p2.iter_mut())
                        .all(|(p1, p2)| p1.equals_up_to_env(p2, env))
            }
            (Type::Func(a1, r1), Type::Func(a2, r2)) => {
                a1.equals_up_to_env(a2, env) && r1.equals_up_to_env(r2, env)
            }

            (Type::Generic(a), Type::Generic(b)) => a == b,

            (Type::TypeVar(a), Type::TypeVar(b)) if a == b => true,

            (a @ Type::TypeVar(_), b @ Type::TypeVar(_))
            | (a @ Type::TypeVar(_), b)
            | (a, b @ Type::TypeVar(_)) => {
                while let Type::TypeVar(x) = a {
                    let x = *x;
                    *a = env.substitutions[x].clone();
                    if matches!(a, Type::TypeVar(y) if x == *y) {
                        break;
                    }
                }

                while let Type::TypeVar(x) = b {
                    let x = *x;
                    *b = env.substitutions[x].clone();
                    if matches!(b, Type::TypeVar(y) if x == *y) {
                        break;
                    }
                }

                match (a, b) {
                    (Type::TypeVar(a), Type::TypeVar(b)) if a == b => true,

                    (Type::TypeVar(a), b) => {
                        env.substitutions[*a] = b.clone();
                        true
                    }

                    (a, Type::TypeVar(b)) => {
                        env.substitutions[*b] = a.clone();
                        true
                    }

                    (a, b) => a.equals_up_to_env(b, env),
                }
            }

            _ => false,
        }
    }

    fn replace_type_vars(&mut self, env: &Environment) -> bool {
        match self {
            Type::Unknown => true,
            Type::Base(BaseType::Named(_, params, _)) => {
                let mut v = true;
                for param in params {
                    v &= param.replace_type_vars(env);
                }
                v
            }

            Type::Base(_) => true,

            Type::Func(a, r) => a.replace_type_vars(env) & r.replace_type_vars(env),

            Type::Refined(_, _) => todo!(),
            Type::Generic(_) => true,

            Type::TypeVar(_) => {
                while let Type::TypeVar(x) = self {
                    let x = *x;
                    *self = env.substitutions[x].clone();
                    if matches!(self, Type::TypeVar(y) if x == *y) {
                        break;
                    }
                }

                if !matches!(self, Type::TypeVar(_)) {
                    self.replace_type_vars(env)
                } else {
                    false
                }
            }
        }
    }

    fn convert_type_vars_to_generics(&mut self, env: &mut Environment, generics: &mut Vec<String>) {
        match self {
            Type::Base(BaseType::Named(_, params, _)) => {
                for param in params {
                    param.convert_type_vars_to_generics(env, generics)
                }
            }

            Type::Func(a, r) => {
                a.convert_type_vars_to_generics(env, generics);
                r.convert_type_vars_to_generics(env, generics);
            }

            Type::Refined(_, _) => todo!(),

            Type::TypeVar(t) => {
                let t = *t;
                let g = format!("a{}", t);
                if !generics.contains(&g) {
                    generics.push(g.clone());
                }

                *self = Type::Generic(g);
                env.substitutions[t] = self.clone();
            }

            _ => (),
        }
    }

    fn convert_generics_to_type_vars(
        &mut self,
        env: &mut Environment,
        generics: &mut HashMap<String, Type>,
    ) {
        match self {
            Type::Base(BaseType::Named(_, params, _)) => {
                for param in params {
                    param.convert_generics_to_type_vars(env, generics)
                }
            }

            Type::Func(a, r) => {
                a.convert_generics_to_type_vars(env, generics);
                r.convert_generics_to_type_vars(env, generics);
            }

            Type::Refined(_, _) => todo!(),

            Type::Generic(g) if !g.starts_with('$') => match generics.entry(g.to_string()) {
                Entry::Occupied(v) => {
                    v.get().clone_into(self);
                }

                Entry::Vacant(v) => {
                    *self = env.new_type_var();
                    v.insert(self.clone());
                }
            },

            _ => (),
        }
    }

    fn has_generic(&self, generics: &Vec<String>) -> bool {
        match self {
            Type::Unknown => false,
            Type::Base(BaseType::Named(_, g, _)) => g.iter().any(|t| t.has_generic(generics)),
            Type::Base(_) => false,
            Type::Func(a, r) => a.has_generic(generics) || r.has_generic(generics),
            Type::Refined(_, _) => todo!(),
            Type::Generic(g) => generics.contains(g),
            Type::TypeVar(_) => false,
        }
    }
}

fn replace_unknowns(env: &mut Environment, ast: &mut Ast) {
    match ast {
        Ast::Binary { left, right, .. } => {
            replace_unknowns(env, left);
            replace_unknowns(env, right);
        }

        Ast::FuncCall { func, args, .. } => {
            replace_unknowns(env, func);
            for arg in args {
                replace_unknowns(env, arg);
            }
        }

        Ast::Let {
            args,
            ret_type,
            value,
            context,
            ..
        } => {
            for (_, arg_type) in args {
                if matches!(arg_type, Type::Unknown) {
                    *arg_type = env.new_type_var();
                }
            }
            if matches!(ret_type, Type::Unknown) {
                *ret_type = env.new_type_var();
            }

            replace_unknowns(env, value);
            replace_unknowns(env, context);
        }

        Ast::TopLet {
            args,
            ret_type,
            value,
            ..
        } => {
            for (_, arg_type) in args {
                if matches!(arg_type, Type::Unknown) {
                    *arg_type = env.new_type_var();
                }
            }
            if matches!(ret_type, Type::Unknown) {
                *ret_type = env.new_type_var();
            }

            replace_unknowns(env, value);
        }

        Ast::If {
            cond, then, elsy, ..
        } => {
            replace_unknowns(env, cond);
            replace_unknowns(env, then);
            replace_unknowns(env, elsy);
        }

        Ast::Match {
            value, patterns, ..
        } => {
            replace_unknowns(env, value);
            for (_, val) in patterns {
                replace_unknowns(env, val);
            }
        }

        Ast::Class { functions, .. } | Ast::Instance { functions, .. } => {
            for func in functions {
                replace_unknowns(env, func);
            }
        }

        _ => (),
    }
}

fn typecheck_helper(env: &mut Environment, ast: &mut Ast, errors: &mut Vec<CheckError>) -> Type {
    match ast {
        Ast::Integer(_, _) => Type::Base(BaseType::I32),
        Ast::Bool(_, _) => Type::Base(BaseType::Bool),

        Ast::Symbol(span, s) => {
            let (_, mut v, mut c) = match env.find_variable(s).cloned() {
                Some(v) => v,
                None => {
                    errors.push(CheckError {
                        message: "undefined identifier".to_string(),
                        primary_label: format!("identifier `{}` has not been previously defined", s),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    return Type::Base(BaseType::Bottom);
                }
            };
            let mut generics = HashMap::new();
            v.convert_generics_to_type_vars(env, &mut generics);
            for (_, c) in c.iter_mut() {
                for c in c {
                    c.convert_generics_to_type_vars(env, &mut generics);
                }
            }
            env.constraints_applied.extend(c);
            v
        }

        Ast::Binary {
            op, left, right, ..
        } => {
            let mut t_left = typecheck_helper(env, left, errors);
            let mut t_right = typecheck_helper(env, right, errors);

            match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                    if t_left.equals_up_to_env(&mut Type::Base(BaseType::I32), env) {
                        if t_right.equals_up_to_env(&mut Type::Base(BaseType::I32), env) {
                            Type::Base(BaseType::I32)
                        } else {
                            errors.push(CheckError {
                                message: "invalid arguments to infix operator".to_string(),
                                primary_label: format!("expected `i32`, found `{}`", t_right),
                                primary_label_loc: right.span(),
                                secondary_labels: Vec::new(),
                                notes: vec![format!("infix op `{}` has type signature `i32 -> i32 -> i32`", op)],
                            });
                            Type::Base(BaseType::Bottom)
                        }
                    } else {
                        errors.push(CheckError {
                            message: "invalid arguments to infix operator".to_string(),
                            primary_label: format!("expected `i32`, found `{}`", t_left),
                            primary_label_loc: left.span(),
                            secondary_labels: Vec::new(),
                            notes: vec![format!("infix op `{}` has type signature `i32 -> i32 -> i32`", op)],
                        });
                        Type::Base(BaseType::Bottom)
                    }
                }

                BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Gt
                | BinaryOp::Ge
                | BinaryOp::Eq
                | BinaryOp::Ne => {
                    if t_left.equals_up_to_env(&mut Type::Base(BaseType::I32), env) {
                        if t_right.equals_up_to_env(&mut Type::Base(BaseType::I32), env) {
                            Type::Base(BaseType::Bool)
                        } else {
                            errors.push(CheckError {
                                message: "invalid arguments to infix operator".to_string(),
                                primary_label: format!("expected `i32`, found `{}`", t_right),
                                primary_label_loc: right.span(),
                                secondary_labels: Vec::new(),
                                notes: vec![format!("infix op `{}` has type signature `i32 -> i32 -> bool`", op)],
                            });
                            Type::Base(BaseType::Bottom)
                        }
                    } else {
                        errors.push(CheckError {
                            message: "invalid arguments to infix operator".to_string(),
                            primary_label: format!("expected `i32`, found `{}`", t_left),
                            primary_label_loc: left.span(),
                            secondary_labels: Vec::new(),
                            notes: vec![format!("infix op `{}` has type signature `i32 -> i32 -> bool`", op)],
                        });
                        Type::Base(BaseType::Bottom)
                    }
                }

                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    if t_left.equals_up_to_env(&mut Type::Base(BaseType::Bool), env) {
                        if t_right.equals_up_to_env(&mut Type::Base(BaseType::Bool), env) {
                            Type::Base(BaseType::Bool)
                        } else {
                            errors.push(CheckError {
                                message: "invalid arguments to infix operator".to_string(),
                                primary_label: format!("expected `bool`, found `{}`", t_right),
                                primary_label_loc: right.span(),
                                secondary_labels: Vec::new(),
                                notes: vec![format!("infix op `{}` has type signature `bool -> bool -> bool`", op)],
                            });
                            Type::Base(BaseType::Bottom)
                        }
                    } else {
                        errors.push(CheckError {
                            message: "invalid arguments to infix operator".to_string(),
                            primary_label: format!("expected `bool`, found `{}`", t_left),
                            primary_label_loc: left.span(),
                            secondary_labels: Vec::new(),
                            notes: vec![format!("infix op `{}` has type signature `bool -> bool -> bool`", op)],
                        });
                        Type::Base(BaseType::Bottom)
                    }
                }
            }
        }

        Ast::FuncCall { func, args, .. } => {
            let mut end = func.span().end;
            let mut t_func = typecheck_helper(env, func, errors);
            let mut it_func = t_func.clone();
            it_func.replace_type_vars(env);
            let it_func = it_func;
            for arg in args {
                let mut t_arg = typecheck_helper(env, arg, errors);

                if t_func.equals_up_to_env(
                    &mut Type::Func(Box::new(env.new_type_var()), Box::new(env.new_type_var())),
                    env,
                ) {
                    while let Type::TypeVar(x) = t_func {
                        let x = x;
                        t_func = env.substitutions[x].clone();
                        if matches!(t_func, Type::TypeVar(y) if x == y) {
                            break;
                        }
                    }
                    if let Type::Func(mut a, r) = t_func {
                        if !a.equals_up_to_env(&mut t_arg, env) {
                            errors.push(CheckError {
                                message: "invalid function application".to_string(),
                                primary_label: format!("expected `{}`, found `{}`", a, t_arg),
                                primary_label_loc: arg.span(),
                                secondary_labels: vec![(format!("function has type `{}`", it_func), func.span())],
                                notes: Vec::new(),
                            });
                        }
                        t_func = *r;
                    } else {
                        t_func = Type::Base(BaseType::Bottom);
                    }
                } else {
                    errors.push(CheckError {
                        message: "invalid function application".to_string(),
                        primary_label: format!("expected a function, found `{}`", t_func),
                        primary_label_loc: func.span().start..end,
                        secondary_labels: vec![("extra argument found here".to_string(), arg.span())],
                        notes: Vec::new(),
                    });
                    t_func = Type::Base(BaseType::Bottom);
                }

                end = arg.span().end;
            }

            t_func
        }

        Ast::Let {
            span,
            mutable: _,
            symbol,
            args,
            ret_type,
            value,
            context,
        } => {
            for (arg, type_) in args.iter() {
                env.push_variable(arg, type_, &[]);
            }
            let mut r = typecheck_helper(env, value, errors);

            for _ in args.iter() {
                env.pop_variable();
            }

            if !ret_type.equals_up_to_env(&mut r, env) {
                errors.push(CheckError {
                    message: "let binding does not match reported type".to_string(),
                    primary_label: format!("expected `{}`, found `{}`", ret_type, r),
                    primary_label_loc: value.span(),
                    secondary_labels: vec![("let binding begins here".to_string(), span.start..span.start + 3)],
                    notes: Vec::new(),
                });
            }
            r = ret_type.clone();

            for (_, arg_type) in args.iter().rev() {
                r = Type::Func(Box::new(arg_type.clone()), Box::new(r));
            }

            env.push_variable(symbol, &r, &[]);
            let t = typecheck_helper(env, context, errors);
            env.pop_variable();
            t
        }

        Ast::EmptyLet { .. } => Type::Unknown,

        Ast::TopLet {
            span,
            args,
            ret_type,
            value,
            ..
        } => {
            let mut generics = HashMap::new();
            for (arg, arg_type) in args.iter() {
                let mut arg_type = arg_type.clone();
                arg_type.convert_generics_to_type_vars(env, &mut generics);
                env.push_variable(arg, &arg_type, &[]);
            }

            let mut t = typecheck_helper(env, value, errors);

            for _ in args.iter() {
                env.pop_variable();
            }

            for (g, mut t) in generics {
                if !t.equals_up_to_env(&mut Type::Generic(g.clone()), env) {
                    errors.push(CheckError {
                        message: "generic unpreserved in let binding".to_string(),
                        primary_label: format!("generic `{}` unpreserved", g),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                }
            }

            if !ret_type.equals_up_to_env(&mut t, env) {
                errors.push(CheckError {
                    message: "let binding does not match reported type".to_string(),
                    primary_label: format!("expected `{}`, found `{}`", ret_type, t),
                    primary_label_loc: value.span(),
                    secondary_labels: vec![("let binding begins here".to_string(), span.start..span.start + 3)],
                    notes: Vec::new(),
                });
            }
            for (_, arg) in args.iter().rev() {
                t = Type::Func(Box::new(arg.clone()), Box::new(t));
            }
            t // TODO: actual type
        }

        Ast::If {
            span, cond, then, elsy,
        } => {
            let mut t_cond = typecheck_helper(env, cond, errors);
            let mut t_then = typecheck_helper(env, then, errors);
            let mut t_elsy = typecheck_helper(env, elsy, errors);

            if !t_cond.equals_up_to_env(&mut Type::Base(BaseType::Bool), env) {
                t_cond.replace_type_vars(env);
                errors.push(CheckError {
                    message: "if condition is not a `bool`".to_string(),
                    primary_label: format!("expected `bool`, found `{}`", t_cond),
                    primary_label_loc: cond.span(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            }

            if t_then.equals_up_to_env(&mut t_elsy, env) {
                t_then
            } else {
                t_then.replace_type_vars(env);
                t_elsy.replace_type_vars(env);
                errors.push(CheckError {
                    message: "if branches don't match".to_string(),
                    primary_label: format!("expected `{}`, found `{}`", t_then, t_elsy),
                    primary_label_loc: span.clone(),
                    secondary_labels: vec![(format!("then branch has type `{}`", t_then), then.span()), (format!("else branch has type `{}`", t_elsy), elsy.span())],
                    notes: Vec::new(),
                });
                Type::Base(BaseType::Bottom)
            }
        }

        Ast::DatatypeDefinition { .. } => Type::Unknown,

        Ast::Match {
            span, value, patterns
        } => {
            let mut t_value = typecheck_helper(env, value, errors);
            let mut result = None;
            let first_span = patterns.first().map(|(_, v)| v.span()).unwrap_or(0..0);

            for (pat, val) in patterns {
                let (mut val_type, append_to_env) = typecheck_pattern(env, pat, errors);
                if !t_value.equals_up_to_env(&mut val_type, env) {
                    t_value.replace_type_vars(env);
                    val_type.replace_type_vars(env);
                    errors.push(CheckError {
                        message: "pattern does not match the type".to_string(),
                        primary_label: format!("expected `{}`, found `{}`", t_value, val_type),
                        primary_label_loc: pat.span(),
                        secondary_labels: vec![(format!("value has type `{}`", t_value), value.span())],
                        notes: Vec::new(),
                    });
                }

                for (var, t) in append_to_env.iter().rev() {
                    env.push_variable(var, t, &[]);
                }

                let mut new = typecheck_helper(env, val, errors);
                match result.as_mut() {
                    None => result = Some(new),

                    Some(t) => {
                        if !t.equals_up_to_env(&mut new, env) {
                            t.replace_type_vars(env);
                            new.replace_type_vars(env);
                            errors.push(CheckError {
                                message: "match cases have mismatched types".to_string(),
                                primary_label: format!("expected `{}`, found `{}`", t, new),
                                primary_label_loc: val.span(),
                                secondary_labels: vec![(format!("value has type `{}`", t), first_span.clone())],
                                notes: Vec::new(),
                            });
                        }
                    }
                }

                for _ in append_to_env {
                    env.pop_variable();
                }
            }

            if let Some(v) = result {
                v
            } else {
                errors.push(CheckError {
                    message: "empty match expression".to_string(),
                    primary_label: "match expression is empty".to_string(),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
                Type::Base(BaseType::Bottom)
            }
        }

        Ast::Class { .. } => {
            Type::Unknown // TODO: actual type
        }

        Ast::Instance {
            name,
            constraints,
            parameters,
            functions,
            ..
        } => {
            if let Some(class) = env.classes.get(name) {
                if class.generics.len() != parameters.len() {
                    todo!()
                }

                let mut class = class.clone();

                let mut gens = HashMap::new();
                for param in parameters.iter_mut() {
                    param.convert_generics_to_type_vars(env, &mut gens);
                }

                for (gen, v) in gens.iter_mut() {
                    if !v.equals_up_to_env(&mut Type::Generic(format!("{}{}", v, gen)), env) {
                        todo!()
                    }
                }

                for (_, ts) in constraints.iter_mut() {
                    for t in ts {
                        t.convert_generics_to_type_vars(env, &mut gens);
                    }
                }

                env.instances
                    .extend(constraints.iter().cloned().map(|(a, b)| (a, b, Vec::new())));

                for param in parameters.iter_mut() {
                    param.replace_type_vars(env);
                }

                for function in functions {
                    match function {
                        Ast::TopLet {
                            symbol,
                            args,
                            ret_type,
                            value,
                            ..
                        } => {
                            if let Some(mut func) = class.funcs.remove(symbol) {
                                let mut generics = HashMap::new();
                                func.convert_generics_to_type_vars(env, &mut generics);
                                for (g, mut v) in generics {
                                    let i = class
                                        .generics
                                        .iter()
                                        .enumerate()
                                        .find(|(_, u)| g == **u)
                                        .map(|(i, _)| i)
                                        .unwrap();
                                    if !parameters[i].equals_up_to_env(&mut v, env) {
                                        todo!()
                                    }
                                }

                                for (name, type_) in args.iter_mut() {
                                    type_.convert_generics_to_type_vars(env, &mut gens);
                                    if let Type::Func(mut a, r) = func {
                                        if !a.equals_up_to_env(type_, env) {
                                            todo!()
                                        }

                                        type_.replace_type_vars(env);
                                        env.push_variable(name, type_, &[]);
                                        func = *r;
                                    } else {
                                        todo!()
                                    }
                                }

                                *ret_type = typecheck_helper(env, &mut **value, errors);
                                for _ in args.iter() {
                                    env.pop_variable();
                                }

                                if !func.equals_up_to_env(ret_type, env) {
                                    todo!()
                                }
                            } else {
                                todo!()
                            }
                        }

                        _ => unreachable!(),
                    }
                }

                if class.funcs.is_empty() {
                    Type::Unknown // TODO: actual type
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }
    }
}

fn typecheck_pattern(
    env: &mut Environment,
    pattern: &mut Pattern,
    errors: &mut Vec<CheckError>,
) -> (Type, Vec<(String, Type)>) {
    match pattern {
        Pattern::Wildcard(_) => (env.new_type_var(), Vec::new()),

        Pattern::Symbol(_, s) => {
            let t = env.new_type_var();
            (t.clone(), vec![(s.clone(), t)])
        }

        Pattern::Constructor(span, name, fields) => {
            if let Some((field_types, type_)) = env.constructors.get(name) {
                if fields.len() != field_types.len() {
                    errors.push(CheckError {
                        message: "constructor applied to wrong number of fields".to_string(),
                        primary_label: format!("constructor `{}` expects {} fields, found {} fields", name, field_types.len(), fields.len()),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });

                    (Type::Base(BaseType::Bottom), Vec::new())
                } else {
                    let mut type_ = type_.clone();
                    let mut field_types = field_types.clone();
                    let mut append_to_env = Vec::new();
                    let mut generics = HashMap::new();
                    type_.convert_generics_to_type_vars(env, &mut generics);
                    for (field, type_) in fields.iter_mut().zip(field_types.iter_mut()) {
                        type_.convert_generics_to_type_vars(env, &mut generics);
                        let (mut t, append) = typecheck_pattern(env, field, errors);
                        append_to_env.extend(append);
                        if !type_.equals_up_to_env(&mut t, env) {
                            t.replace_type_vars(env);
                            errors.push(CheckError {
                                message: "constructor field has incompatible type".to_string(),
                                primary_label: format!("field expects `{}`, found `{}`", type_, t),
                                primary_label_loc: span.clone(),
                                secondary_labels: Vec::new(),
                                notes: Vec::new(),
                            });
                        }
                    }
                    type_.replace_type_vars(env);

                    let mut set = HashSet::new();
                    let mut dup_indices = Vec::new();
                    for (i, (s, _)) in append_to_env.iter().enumerate() {
                        if !set.insert(s) {
                            dup_indices.push(i);
                            errors.push(CheckError {
                                message: "pattern has duplicate variables".to_string(),
                                primary_label: format!("duplicate variable `{}` found", s),
                                primary_label_loc: span.clone(),
                                secondary_labels: Vec::new(),
                                notes: Vec::new(),
                            });
                        }
                    }

                    for i in dup_indices.into_iter().rev() {
                        append_to_env.remove(i);
                    }

                    (type_, append_to_env)
                }
            } else {
                errors.push(CheckError {
                    message: "constructor doesn't exist".to_string(),
                    primary_label: format!("constructor `{}` doesn't exist", name),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });

                (Type::Base(BaseType::Bottom), Vec::new())
            }
        }

        Pattern::SymbolOrUnitConstructor(span, s) => {
            if let Some((fields, type_)) = env.constructors.get(s) {
                if fields.is_empty() {
                    *pattern = Pattern::Constructor(span.clone(), s.clone(), Vec::new());
                    let mut type_ = type_.clone();
                    type_.convert_generics_to_type_vars(env, &mut HashMap::new());
                    (type_, Vec::new())
                } else {
                    errors.push(CheckError {
                        message: "constructor applied to wrong number of fields".to_string(),
                        primary_label: format!("constructor `{}` expects {} fields, found {} fields", s, fields.len(), 0),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });

                    (Type::Base(BaseType::Bottom), Vec::new())
                }
            } else {
                let s = s.clone();
                *pattern = Pattern::Symbol(span.clone(), s.clone());
                let t = env.new_type_var();
                (t.clone(), vec![(s, t)])
            }
        }

        Pattern::As(span, s, pat) => {
            let (t, mut append) = typecheck_pattern(env, pat, errors);

            if append.iter().any(|(v, _)| v == s) {
                errors.push(CheckError {
                    message: "pattern has duplicate variables".to_string(),
                    primary_label: format!("duplicate variable `{}` found", s),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            } else {
                append.push((s.clone(), t.clone()));
            }

            (t, append)
        }

        Pattern::Or(_, pats) => {
            let first = pats.first_mut().unwrap();
            let first_span = first.span();
            let (mut t, mut append) = typecheck_pattern(env, first, errors);
            let mut append_span = first_span.clone();
            for pat in pats.iter_mut().skip(1) {
                let (mut t2, mut append2) = typecheck_pattern(env, pat, errors);
                if !t.equals_up_to_env(&mut t2, env) {
                    errors.push(CheckError {
                        message: "patterns are not of the same type".to_string(),
                        primary_label: format!("expected pattern of type `{}`, found type `{}`", t, t2),
                        primary_label_loc: pat.span(),
                        secondary_labels: vec![(format!("pattern has type `{}`", t), first_span.clone())],
                        notes: Vec::new(),
                    });
                }

                let mut append2_span = pat.span();
                if append.len() < append2.len() {
                    std::mem::swap(&mut append, &mut append2);
                    std::mem::swap(&mut append_span, &mut append2_span);
                }

                for (s, t) in append.iter_mut().rev() {
                    if let Some((_, t2)) = append2.iter_mut().rev().find(|(s2, _)| s2 == s) {
                        if !t.equals_up_to_env(t2, env) {
                            errors.push(CheckError {
                                message: "variable does not share the same type among patterns".to_string(),
                                primary_label: format!("expected type `{}`, found type `{}`", t, t2),
                                primary_label_loc: append2_span.clone(),
                                secondary_labels: vec![(format!("`{}` has type `{}` in this pattern", s, t), append_span.clone())],
                                notes: Vec::new(),
                            })
                        }
                    } else {
                        errors.push(CheckError {
                            message: "unshared variable among patterns".to_string(),
                            primary_label: format!("expected to find variable `{}` in this pattern; not found", s),
                            primary_label_loc: append2_span.clone(),
                            secondary_labels: vec![(format!("`{}` found in this pattern", s), append_span.clone())],
                            notes: Vec::new(),
                        })
                    }
                }
            }

            (t, append)
        }
    }
}

fn replace_type_vars(ast: &mut Ast, env: &mut Environment, errors: &mut Vec<CheckError>) {
    match ast {
        Ast::Binary { left, right, .. } => {
            replace_type_vars(left, env, errors);
            replace_type_vars(right, env, errors);
        }

        Ast::FuncCall { func, args, .. } => {
            replace_type_vars(func, env, errors);
            for arg in args {
                replace_type_vars(arg, env, errors);
            }
        }

        Ast::Let {
            args,
            ret_type,
            value,
            context,
            ..
        } => {
            for (_, arg_type) in args.iter_mut() {
                if !arg_type.replace_type_vars(env) {
                    todo!()
                }
            }
            if !ret_type.replace_type_vars(env) {
                todo!()
            }

            replace_type_vars(value, env, errors);
            replace_type_vars(context, env, errors);
        }

        Ast::TopLet {
            args,
            ret_type,
            value,
            constraints,
            generics,
            ..
        } => {
            for (_, arg_type) in args.iter_mut() {
                arg_type.replace_type_vars(env);
                arg_type.convert_type_vars_to_generics(env, generics);
            }
            ret_type.replace_type_vars(env);
            ret_type.convert_type_vars_to_generics(env, generics);

            if !generics.is_empty() {
                let mut extension = Vec::new();
                for (name, params) in env.constraints_applied.iter() {
                    let params: Vec<_> = params
                        .iter()
                        .cloned()
                        .map(|mut t| {
                            t.replace_type_vars(env);
                            t
                        })
                        .collect();
                    extension.push((name.clone(), params));
                }

                for (name, mut params) in extension {
                    for param in params.iter_mut() {
                        param.convert_type_vars_to_generics(env, &mut Vec::new());
                    }

                    if params.iter().any(|t| t.has_generic(&generics)) {
                        constraints.push((name, params));
                    }
                }
            }

            replace_type_vars(value, env, errors)
        }

        Ast::If {
            cond, then, elsy, ..
        } => {
            replace_type_vars(cond, env, errors);
            replace_type_vars(then, env, errors);
            replace_type_vars(elsy, env, errors);
        }

        Ast::Match {
            value, patterns, ..
        } => {
            replace_type_vars(value, env, errors);
            for (_, result) in patterns {
                replace_type_vars(result, env, errors);
            }
        }

        Ast::Class { functions, .. } | Ast::Instance { functions, .. } => {
            for func in functions {
                replace_type_vars(func, env, errors);
            }
        }

        _ => (),
    }
}

pub fn typecheck(asts: &mut [Ast]) -> Result<(), Vec<CheckError>> {
    let mut env = Environment::default();
    let mut errors = Vec::new();

    for ast in asts.iter_mut() {
        replace_unknowns(&mut env, ast);
    }

    for ast in asts.iter() {
        match ast {
            Ast::TopLet {
                symbol,
                args,
                ret_type,
                ..
            } => {
                let mut top = ret_type.clone();
                for (_, arg_type) in args.iter().rev() {
                    top = Type::Func(Box::new(arg_type.clone()), Box::new(top));
                }
                env.push_variable(symbol, &top, &[]);
            }

            Ast::DatatypeDefinition {
                name,
                generics,
                variants,
                ..
            } => {
                for (cons_name, fields) in variants {
                    let mut constructor = Vec::new();
                    let mut top = Type::Base(BaseType::Named(
                        name.clone(),
                        generics.iter().cloned().map(Type::Generic).collect(),
                        Vec::new(),
                    ));
                    let type_ = top.clone();
                    for (_, type_) in fields.iter().rev() {
                        top = Type::Func(Box::new(type_.clone()), Box::new(top));
                    }
                    env.push_variable(cons_name, &top, &[]);
                    for (_, type_) in fields {
                        constructor.push(type_.clone());
                    }
                    env.constructors
                        .insert(cons_name.clone(), (constructor, type_));
                }
            }

            Ast::Class {
                name,
                generics,
                constraints,
                functions,
                ..
            } => {
                let mut class_funcs = HashMap::new();
                let mut constraints = constraints.clone();
                constraints.push((
                    name.clone(),
                    generics.iter().cloned().map(Type::Generic).collect(),
                ));
                for func in functions {
                    match func {
                        Ast::EmptyLet {
                            symbol,
                            args,
                            ret_type,
                            ..
                        } => {
                            let mut top = ret_type.clone();
                            for (_, type_) in args.iter().rev() {
                                top = Type::Func(Box::new(type_.clone()), Box::new(top));
                            }
                            class_funcs.insert(symbol.clone(), top);
                        }

                        _ => (),
                    }
                }

                for (a, b) in class_funcs.iter() {
                    env.push_variable(a, b, &constraints);
                }

                env.classes.insert(
                    name.to_string(),
                    Class {
                        generics: generics.clone(),
                        constraints,
                        funcs: class_funcs,
                    },
                );
            }

            Ast::Instance {
                name,
                parameters,
                constraints,
                ..
            } => {
                env.instances
                    .push((name.clone(), parameters.clone(), constraints.clone()));
            }

            _ => (),
        }
    }

    for ast in asts.iter_mut() {
        typecheck_helper(&mut env, ast, &mut errors);
    }

    env.check_constraints(&mut errors);
    for ast in asts.iter_mut() {
        replace_type_vars(ast, &mut env, &mut errors);
    }
    env.update_vars();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
