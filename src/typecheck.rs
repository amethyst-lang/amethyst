use std::collections::{HashMap, HashSet, hash_map::Entry};

use crate::{lexer::Span, parse::{Ast, Type, Monotype, BaseType, Pattern}};

#[derive(Debug)]
pub struct CheckError {
    pub message: String,
    pub primary_label: String,
    pub primary_label_loc: Span,
    pub secondary_labels: Vec<(String, Span)>,
    pub notes: Vec<String>,
}

#[derive(Debug)]
struct Instance {
    generics: Vec<String>,
    parameters: Vec<Monotype>,
    constraints: Vec<(String, Vec<Monotype>)>,
}

#[derive(Debug)]
struct Class {
    parameter_count: usize,
    instances: Vec<Instance>,
    superclasses: HashMap<String, Vec<usize>>,
}

#[derive(Default, Debug)]
struct Environment {
    variables: Vec<(String, Type)>,
    constructors: HashMap<String, Type>,
    // types_defined: HashMap<String, Type>, // TODO: check if a type used was previously defined
    classes: HashMap<String, Class>,

    substitutions: Vec<Monotype>,
    class_constraints: Vec<Vec<(String, usize)>>, // kinda silly but states that, for a given type variable t, it is the ith argument of some instance of the given class
}

impl Environment {
    fn init_defaults(mut self) -> Self {
        self.variables.push(("+".to_string(), Type {
            foralls: Vec::new(),
            constraints: Vec::new(),
            monotype: Monotype::Func(Box::new(Monotype::Base(BaseType::I32)), Box::new(Monotype::Func(Box::new(Monotype::Base(BaseType::I32)), Box::new(Monotype::Base(BaseType::I32))))),
        }));
        self
    }

    fn push_var(&mut self, var: String, type_: Type) {
        self.variables.push((var, type_));
    }

    fn find_var(&mut self, var: &str) -> Option<Monotype> {
        for (key, type_) in self.variables.iter().rev() {
            if key == var {
                return Some(type_.clone().instantiate(self));
            }
        }

        return None;
    }

    fn find_var_no_inst(&self, var: &str) -> Option<Type> {
        self.variables.iter().rev().find(|(v, _)| v == var).map(|(_, t)| t.clone())
    }

    fn find_cons(&mut self, cons: &str) -> Option<Monotype> {
        self.constructors.get(cons).cloned().map(|v| v.instantiate(self))
    }

    fn pop_var(&mut self) -> Option<(String, Type)> {
        self.variables.pop()
    }

    fn new_var(&mut self) -> Monotype {
        let t = Monotype::TypeVar(self.substitutions.len());
        self.substitutions.push(t.clone());
        self.class_constraints.push(Vec::new());
        t
    }

    fn find(&self, t: &Monotype) -> Monotype {
        let mut t = t.clone();
        while let Monotype::TypeVar(i) = t {
            let t_new = &self.substitutions[i];
            if matches!(t_new, &Monotype::TypeVar(j) if i == j) {
                break;
            }
            t = t_new.clone();
        }

        match t {
            Monotype::Base(BaseType::Named(s, ts)) => Monotype::Base(BaseType::Named(s, ts.into_iter().map(|v| self.find(&v)).collect())),
            Monotype::Func(a, r) => Monotype::Func(Box::new(self.find(&a)), Box::new(self.find(&r))),
            t => t
        }
    }

    fn unify(&mut self, ta: &mut Monotype, tb: &mut Monotype) -> bool {
        *ta = self.find(ta);
        *tb = self.find(tb);

        match (ta, tb) {
            (Monotype::Base(BaseType::Bottom), _) | (_, Monotype::Base(BaseType::Bottom)) => true,
            (ta, tb) if ta == tb => true,

            (Monotype::Base(BaseType::Named(c1, ts1)), Monotype::Base(BaseType::Named(c2, ts2))) if c1 == c2 && ts1.len() == ts2.len() => {
                ts1.iter_mut().zip(ts2.iter_mut()).all(|(a, b)| self.unify(a, b))
            }

            (Monotype::Func(a1, r1), Monotype::Func(a2, r2)) => {
                self.unify(a1, a2) && self.unify(r1, r2)
            }

            (v @ Monotype::TypeVar(_), t) | (t, v @ Monotype::TypeVar(_)) => {
                if let Monotype::TypeVar(i) = v {
                    self.substitutions[*i] = t.clone();
                } else {
                    unreachable!();
                }

                *v = t.clone();
                true
            }

            _ => false,
        }
    }

    fn update_all_var_types(&mut self) {
        let mut variables = Vec::new();
        std::mem::swap(&mut variables, &mut self.variables);
        for (_, t) in variables.iter_mut() {
            t.monotype = self.find(&t.monotype);
            t.polymorphise(self);
        }
        std::mem::swap(&mut variables, &mut self.variables);
    }
}

impl Type {
    fn instantiate(self, env: &mut Environment) -> Monotype {
        let mut replacements = HashMap::new();
        for generic in self.foralls {
            replacements.insert(generic.clone(), env.new_var());
        }

        let mut constraint_map = HashMap::new();
        for (class_name, params) in self.constraints {
            // TODO: error handling
            if let Some(class) = env.classes.get(&class_name) {
                for (i, param) in params.into_iter().enumerate() {
                    param.instantiate(&replacements).create_typevar_constraints(i, &class_name, class, &mut constraint_map);
                }
            }
        }

        for (i, vals) in constraint_map {
            env.class_constraints[i].extend(vals);
        }

        self.monotype.instantiate(&replacements)
    }

    fn polymorphise(&mut self, env: &Environment) {
        let mut new_generics = HashSet::new();
        self.monotype.polymorphise(env, &mut new_generics);

        for new in new_generics {
            self.foralls.push(new);
        }
    }
}

impl Monotype {
    fn create_typevar_constraints(self, i: usize, class_name: &str, class: &Class, constraint_map: &mut HashMap<usize, Vec<(String, usize)>>) {
        match self {
            Monotype::Base(BaseType::Named(_, ts)) => {
                for t in ts {
                    t.create_typevar_constraints(i, class_name, class, constraint_map);
                }
            }

            Monotype::Func(a, r) => {
                a.create_typevar_constraints(i, class_name, class, constraint_map);
                r.create_typevar_constraints(i, class_name, class, constraint_map);
            }

            Monotype::TypeVar(j) => {
                match constraint_map.entry(j) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push((class_name.to_string(), i));
                    }

                    Entry::Vacant(v) => {
                        v.insert(vec![(class_name.to_string(), i)]);
                    }
                }
            }

            _ => (),
        }
    }

    fn instantiate(self, replacements: &HashMap<String, Monotype>) -> Self {
        match self {
            Monotype::Unknown => self,

            Monotype::Base(BaseType::Named(name, ts)) => {
                Monotype::Base(BaseType::Named(name, ts.into_iter().map(|v| v.instantiate(replacements)).collect()))
            }

            Monotype::Base(_) => self,

            Monotype::Func(a, r) => {
                Monotype::Func(Box::new(a.instantiate(replacements)), Box::new(r.instantiate(replacements)))
            }

            Monotype::Generic(g) => {
                replacements.get(&g).expect("all generics must have been counted for in a universal type").clone()
            }

            Monotype::TypeVar(_) => self,
        }
    }

    fn fresh(&mut self, env: &mut Environment, type_vars: &mut HashMap<usize, usize>) {
        match self {
            Monotype::Base(BaseType::Named(_, ts)) => {
                for t in ts {
                    t.fresh(env, type_vars);
                }
            }

            Monotype::Func(a, r) => {
                a.fresh(env, type_vars);
                r.fresh(env, type_vars);
            }

            Monotype::TypeVar(i) => {
                let i = *i;

                match type_vars.entry(i) {
                    Entry::Occupied(v) => {
                        *self = Monotype::TypeVar(*v.get());
                    }

                    Entry::Vacant(v) => {
                        let t = env.new_var();
                        if let Monotype::TypeVar(j) = t {
                            v.insert(j);
                        }
                        *self = t;
                    }
                }
            }

            _ => (),
        }
    }

    fn polymorphise(&mut self, env: &Environment, new_generics: &mut HashSet<String>) {
        match self {
            Monotype::Base(BaseType::Named(_, ts)) => {
                for t in ts {
                    t.polymorphise(env, new_generics);
                }
            }

            Monotype::Func(a, r) => {
                a.polymorphise(env, new_generics);
                r.polymorphise(env, new_generics);
            }

            Monotype::TypeVar(i) => {
                let i = *i;
                *self = env.find(self);
                if let Monotype::TypeVar(_) = self {
                    let new = format!("a${}", i);
                    *self = Monotype::Generic(new.clone());
                    new_generics.insert(new);
                } else {
                    self.polymorphise(env, new_generics);
                }
            }

            _ => (),
        }
    }
}

fn typecheck_helper(ast: &mut Ast, env: &mut Environment, errors: &mut Vec<CheckError>) -> Monotype {
    match ast {
        Ast::Integer(_, _) => Monotype::Base(BaseType::I32),
        Ast::Bool(_, _) => Monotype::Base(BaseType::Bool),

        Ast::Symbol(span, s) => {
            if let Some(t) = env.find_var(s) {
                t
            } else {
                errors.push(CheckError {
                    message: "symbol not found".to_string(),
                    primary_label: format!("symbol `{}` was not previously declared", s),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
                Monotype::Base(BaseType::Bottom)
            }
        }

        Ast::Binary { span, op, left, right } => {
            if let Some(op_type) = env.find_var(op) {
                let mut op_type = op_type.clone();
                let t_left = typecheck_helper(left, env, errors);
                let t_right = typecheck_helper(right, env, errors);
                let ret = env.new_var();
                if env.unify(&mut op_type, &mut Monotype::Func(Box::new(t_left.clone()), Box::new(Monotype::Func(Box::new(t_right.clone()), Box::new(ret.clone()))))) {
                    env.find(&ret)
                } else {
                    errors.push(CheckError {
                        message: "cannot unify op and arguments".to_string(),
                        primary_label: format!("operator `{}` has type `{}`, whereas arguments are of types `{}` and `{}`", op, op_type, t_left, t_right),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    Monotype::Base(BaseType::Bottom)
                }
            } else {
                errors.push(CheckError {
                    message: "op is undefined".to_string(),
                    primary_label: format!("operator `{}` is undefined", op),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
                Monotype::Base(BaseType::Bottom)
            }
        }

        Ast::FuncCall { span, func, args } => {
            let mut t_func = typecheck_helper(func, env, errors);
            for arg in args {
                let t_arg = typecheck_helper(arg, env, errors);
                let ret = env.new_var();
                if env.unify(&mut t_func, &mut Monotype::Func(Box::new(t_arg.clone()), Box::new(ret.clone()))) {
                    if let Monotype::Func(_, r) = t_func {
                        t_func = env.find(&*r);
                    }
                } else {
                    errors.push(CheckError {
                        message: "invalid function application".to_string(),
                        primary_label: format!("applied value of type `{}` to argument of type `{}`", t_func, t_arg),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    t_func = Monotype::Base(BaseType::Bottom);
                }
            }

            t_func
        }

        Ast::Let { symbol, args, value, context, .. } => {
            for arg in args.iter() {
                let monotype = env.new_var();
                env.push_var(arg.clone(), Type {
                    foralls: Vec::new(),
                    constraints: Vec::new(),
                    monotype,
                });
            }

            let mut monotype = typecheck_helper(value, env, errors);
            for _ in args.iter() {
                let (_, a) = env.pop_var().expect("variable was not popped");
                monotype = Monotype::Func(Box::new(a.monotype), Box::new(monotype));
            }

            env.push_var(symbol.clone(), Type {
                foralls: Vec::new(),
                constraints: Vec::new(),
                monotype,
            });
            let result = typecheck_helper(context, env, errors);
            env.pop_var();
            result
        }

        Ast::EmptyLet { .. } => Monotype::Base(BaseType::Bottom),

        Ast::TopLet { span, symbol, args, value } => {
            for arg in args.iter() {
                let monotype = env.new_var();
                env.push_var(arg.to_string(), Type {
                    foralls: Vec::new(),
                    constraints: Vec::new(),
                    monotype,
                });
            }

            let mut t = typecheck_helper(value, env, errors);

            for _ in args.iter().rev() {
                let (_, a) = env.pop_var().expect("argument was never popped");
                t = Monotype::Func(Box::new(a.monotype), Box::new(t));
            }

            let mut old = env.variables.iter().rev().find(|(n, _)| n == symbol).map(|(_, t)| t.clone()).expect("top level bindings are defined before typechecking starts");
            if !old.foralls.is_empty() {
                let mut t = env.find(&t);
                t.fresh(env, &mut HashMap::new());
                if !env.unify(&mut old.monotype, &mut t) {
                    errors.push(CheckError {
                        message: "top level binding cannot be typechecked".to_string(),
                        primary_label: format!("`{}` has both types `{}` and `{}`", symbol, old, t),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                }
            }

            let mut old = env.find_var(symbol).expect("top level bindings are defined before typechecking starts");
            if !env.unify(&mut t, &mut old) {
                errors.push(CheckError {
                    message: "top level binding cannot be typechecked".to_string(),
                    primary_label: format!("`{}` has both types `{}` and `{}`", symbol, old, t),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            }

            Monotype::Base(BaseType::Bottom)
        }

        Ast::If { span, cond, then, elsy } => {
            let mut t_cond = typecheck_helper(cond, env, errors);
            if !env.unify(&mut t_cond, &mut Monotype::Base(BaseType::Bool)) {
                errors.push(CheckError {
                    message: "expected value of type `bool` in condition of if expression".to_string(),
                    primary_label: format!("value has type `{}` instead of `bool`", t_cond),
                    primary_label_loc: cond.span(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            }

            let mut t_then = typecheck_helper(then, env, errors);
            let mut t_elsy = typecheck_helper(elsy, env, errors);

            if env.unify(&mut t_then, &mut t_elsy) {
                env.find(&t_then)
            } else {
                errors.push(CheckError {
                    message: "branches of if expression do not match".to_string(),
                    primary_label: format!("`{}` and `{}` are not equivalent types", t_then, t_elsy),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
                Monotype::Base(BaseType::Bottom)
            }
        }

        Ast::DatatypeDefinition { .. } => Monotype::Base(BaseType::Bottom),

        Ast::Match { span, value, patterns } => {
            let mut t_value = typecheck_helper(value, env, errors);
            let mut t = env.new_var();

            for (pat, body) in patterns.iter_mut() {
                let (mut t_pat, vars) = typecheck_pattern(pat, env, errors);
                if !env.unify(&mut t_pat, &mut t_value) {
                    errors.push(CheckError {
                        message: "incompatible types among pattern and value".to_string(),
                        primary_label: format!("pattern has type `{}` but value has incompatible type `{}`", t_pat, t_value),
                        primary_label_loc: pat.span(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                }

                for (var, monotype) in vars.iter().cloned() {
                    env.push_var(var, Type {
                        foralls: Vec::new(),
                        constraints: Vec::new(),
                        monotype,
                    });
                }

                let mut t_body = typecheck_helper(body, env, errors);
                if !env.unify(&mut t, &mut t_body) {
                    errors.push(CheckError {
                        message: "match expression has incompatible types in branches".to_string(),
                        primary_label: format!("two branches have types `{}` and `{}`", t, t_body),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                }

                for _ in vars {
                    env.pop_var();
                }
            }

            t
        }

        Ast::Class { span, name, generics, constraints, functions } => {
            // TODO
            Monotype::Base(BaseType::Bottom)
        }

        // TODO: dealing with constraints
        Ast::Instance { parameters, functions, .. } => {
            for func in functions.iter_mut() {
                if let Ast::TopLet { span, symbol, args, value } = func {
                    for arg in args.iter() {
                        let monotype = env.new_var();
                        env.push_var(arg.to_string(), Type {
                            foralls: Vec::new(),
                            constraints: Vec::new(),
                            monotype,
                        });
                    }

                    let mut t = typecheck_helper(value, env, errors);

                    for _ in args.iter().rev() {
                        let (_, a) = env.pop_var().expect("argument was never popped");
                        t = Monotype::Func(Box::new(a.monotype), Box::new(t));
                    }

                    let old = env.find_var_no_inst(symbol).expect("top level bindings are defined before typechecking starts");
                    let replacements = old.foralls.iter().cloned().zip(parameters.iter().cloned()).collect();
                    let mut old = old.monotype.instantiate(&replacements);
                    if !env.unify(&mut t, &mut old) {
                        errors.push(CheckError {
                            message: "top level binding cannot be typechecked".to_string(),
                            primary_label: format!("`{}` has both types `{}` and `{}`", symbol, old, t),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                    }
                }
            }

            Monotype::Base(BaseType::Bottom)
        }
    }
}

fn typecheck_pattern(pattern: &mut Pattern, env: &mut Environment, errors: &mut Vec<CheckError>) -> (Monotype, Vec<(String, Monotype)>) {
    match pattern {
        Pattern::Wildcard(_) => (Monotype::Base(BaseType::Bottom), Vec::new()),

        Pattern::Symbol(_, s) => {
            let t = env.new_var();
            (t.clone(), vec![(s.clone(), t)])
        }

        Pattern::Constructor(span, cons, pats) => {
            if let Some(mut t_cons) = env.find_cons(cons) {
                let mut new_env = Vec::new();
                for pat in pats {
                    let (mut t_pat, addition) = typecheck_pattern(pat, env, errors);

                    for new in addition {
                        if new_env.iter().any(|(v, _)| *v == new.0) {
                            errors.push(CheckError {
                                message: "variable declared multiple times in pattern".to_string(),
                                primary_label: format!("variable `{}` declared at least twice", new.0),
                                primary_label_loc: span.clone(),
                                secondary_labels: Vec::new(),
                                notes: Vec::new(),
                            });
                            continue;
                        }

                        new_env.push(new);
                    }

                    if let Monotype::Func(mut t_arg, t_ret) = t_cons {
                        if !env.unify(&mut t_pat, &mut t_arg) {
                            errors.push(CheckError {
                                message: "pattern type does not match constructor".to_string(),
                                primary_label: format!("constructor argument has type `{}` but type `{}` was provided", t_arg, t_pat),
                                primary_label_loc: span.clone(),
                                secondary_labels: Vec::new(),
                                notes: Vec::new(),
                            });
                        }
                        t_cons = *t_ret;
                    } else {
                        errors.push(CheckError {
                            message: "too few arguments in pattern".to_string(),
                            primary_label: "constructor has more arguments than pattern".to_string(),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                        return (Monotype::Base(BaseType::Bottom), Vec::new());
                    }
                }

                if let Monotype::Func(_, _) = t_cons {
                    errors.push(CheckError {
                        message: "too many arguments in pattern".to_string(),
                        primary_label: "constructor has fewer arguments than pattern".to_string(),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    (Monotype::Base(BaseType::Bottom), new_env)
                } else {
                    (t_cons, new_env)
                }
            } else {
                (Monotype::Base(BaseType::Bottom), Vec::new())
            }
        }

        Pattern::SymbolOrUnitConstructor(_, name) => {
            if let Some(t_cons) = env.find_cons(name) {
                if let Monotype::Func(_, _) = t_cons {
                    let t = env.new_var();
                    (t.clone(), vec![(name.clone(), t)])
                } else {
                    (t_cons, Vec::new())
                }
            } else {
                let t = env.new_var();
                (t.clone(), vec![(name.clone(), t)])
            }
        }

        Pattern::As(span, name, pat) => {
            let (t_pat, mut new_env) = typecheck_pattern(pat, env, errors);
            if new_env.iter().any(|(v, _)| v == name) {
                errors.push(CheckError {
                    message: "variable declared multiple times in pattern".to_string(),
                    primary_label: format!("variable `{}` declared at least twice", name),
                    primary_label_loc: span.clone(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            } else {
                new_env.push((name.clone(), t_pat.clone()));
            }

            (t_pat, new_env)
        }

        Pattern::Or(span, cases) => {
            let mut t = Monotype::Base(BaseType::Bottom);
            let mut new_env = Vec::new();
            let mut first = true;
            for case in cases {
                if first {
                    (t, new_env) = typecheck_pattern(case, env, errors);
                    first = false;
                    continue;
                }

                let (mut t_case, mut variant) = typecheck_pattern(case, env, errors);
                if !env.unify(&mut t, &mut t_case) {
                    errors.push(CheckError {
                        message: "or pattern has incompatible types".to_string(),
                        primary_label: format!("found incompatible types `{}` and `{}`", t, t_case),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    return (Monotype::Base(BaseType::Bottom), Vec::new())
                }

                if variant.len() != new_env.len() {
                    errors.push(CheckError {
                        message: "incompatible variable count".to_string(),
                        primary_label: "or pattern has inconsistent number of variables".to_string(),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    return (Monotype::Base(BaseType::Bottom), Vec::new())
                }

                for (n1, t1) in new_env.iter_mut() {
                    if !variant.iter_mut().any(|(n2, t2)| n1 == n2 && env.unify(t1, t2)) {
                        errors.push(CheckError {
                            message: "unshared variable found in or pattern".to_string(),
                            primary_label: format!("variable `{}` not shared amongst patterns", n1),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                        return (Monotype::Base(BaseType::Bottom), Vec::new())
                    }
                }
            }

            (t, new_env)
        }
    }
}

pub fn typecheck(asts: &mut [Ast]) -> Result<HashMap<String, Type>, Vec<CheckError>> {
    let mut env = Environment::default().init_defaults();
    let mut errors = Vec::new();

    for ast in asts.iter() {
        match ast {
            Ast::EmptyLet { span, symbol, type_, .. } => {
                if let Some((_, t)) = env.variables.iter_mut().rev().find(|(n, _)| n == symbol) {
                    let mut type_ = type_.clone();
                    std::mem::swap(&mut type_, t);
                    let mut t = env.find_var(symbol).expect("we literally just had this");
                    if !env.unify(&mut t, &mut type_.monotype) {
                        errors.push(CheckError {
                            message: "top level definition has conflicting types".to_string(),
                            primary_label: format!("`{}` has both types `{}` and `{}`", symbol, t, type_),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                    }
                } else {
                    env.push_var(symbol.clone(), type_.clone());
                }
            }

            Ast::TopLet { span, symbol, args, .. } => {
                let mut monotype = env.new_var();
                for _ in args {
                    monotype = Monotype::Func(Box::new(env.new_var()), Box::new(monotype));
                }

                let mut type_ = Type {
                    foralls: Vec::new(),
                    constraints: Vec::new(),
                    monotype,
                };

                if let Some(mut t) = env.find_var(symbol) {
                    if !env.unify(&mut t, &mut type_.monotype) {
                        errors.push(CheckError {
                            message: "top level definition has conflicting types".to_string(),
                            primary_label: format!("`{}` has both types `{}` and `{}`", symbol, t, type_),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                    }
                } else {
                    env.push_var(symbol.clone(), type_.clone());
                }
            }

            Ast::DatatypeDefinition { name, constraints, generics, variants, .. } => {
                let t = Type {
                    foralls: generics.clone(),
                    constraints: constraints.clone(),
                    monotype: Monotype::Base(BaseType::Named(name.clone(), generics.iter().map(|v| Monotype::Generic(v.clone())).collect())),
                };

                for (cons, fields) in variants {
                    let mut monotype = t.monotype.clone();
                    for (_, field_type) in fields.iter().rev() {
                        monotype = Monotype::Func(Box::new(field_type.clone()), Box::new(monotype));
                    }

                    let type_ = Type {
                        foralls: t.foralls.clone(),
                        constraints: t.constraints.clone(),
                        monotype,
                    };
                    env.constructors.insert(cons.clone(), type_.clone());
                    env.push_var(cons.clone(), type_);
                }
            }

            Ast::Class { span, name, generics, constraints, functions } => {
                let mut class = Class {
                    parameter_count: generics.len(),
                    instances: Vec::new(),
                    superclasses: HashMap::new(),
                };

                'a: for (superclass, s_generics) in constraints {
                    if !env.classes.contains_key(superclass) {
                        errors.push(CheckError {
                            message: "superclass is undefined".to_string(),
                            primary_label: format!("superclass `{}` was referenced here", superclass),
                            primary_label_loc: span.clone(),
                            secondary_labels: Vec::new(),
                            notes: Vec::new(),
                        });
                        continue;
                    }

                    let mut built = Vec::new();
                    for g in s_generics {
                        let Some((i, _)) = generics.iter().enumerate().find(|(_, s)| *s == g)
                        else {
                            errors.push(CheckError {
                                message: "typevariable not found".to_string(),
                                primary_label: format!("typevariable `{}` never defined", g),
                                primary_label_loc: span.clone(),
                                secondary_labels: Vec::new(),
                                notes: Vec::new(),
                            });
                            continue 'a;
                        };
                        built.push(i);
                    }

                    class.superclasses.insert(superclass.clone(), built);
                }

                let Entry::Vacant(entry) = env.classes.entry(name.clone())
                else {
                    errors.push(CheckError {
                        message: "class defined multiple times".to_string(),
                        primary_label: format!("class `{}` defined a second time here", name),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    continue;
                };

                entry.insert(class);

                for func in functions {
                    if let Ast::EmptyLet { symbol, type_, .. } = func {
                        env.push_var(symbol.clone(), type_.clone());
                    }
                }
            }

            Ast::Instance { span, name, generics, parameters, constraints, .. } => {
                let instance = Instance {
                    generics: generics.clone(),
                    parameters: parameters.clone(),
                    constraints: constraints.clone(),
                };

                let Some(class) = env.classes.get_mut(name)
                else {
                    errors.push(CheckError {
                        message: "attempted to create instance of nonexistent class".to_string(),
                        primary_label: format!("class `{}` is undefined", name),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    continue;
                };

                if parameters.len() != class.parameter_count {
                    errors.push(CheckError {
                        message: "mismatched number of parameters".to_string(),
                        primary_label: format!("class `{}` expects {} parameters, received {} parameters", name, class.parameter_count, parameters.len()),
                        primary_label_loc: span.clone(),
                        secondary_labels: Vec::new(),
                        notes: Vec::new(),
                    });
                    continue;
                }

                class.instances.push(instance);
            }

            _ => (),
        }
    }

    for ast in asts.iter_mut() {
        typecheck_helper(ast, &mut env, &mut errors);
    }

    env.update_all_var_types();

    if errors.is_empty() {
        Ok(env.variables.into_iter().collect())
    } else {
        Err(errors)
    }
}
