use std::collections::HashMap;

use crate::{lexer::Span, parse::{Ast, Type, Monotype, BaseType}};

#[derive(Debug)]
pub struct CheckError {
    pub message: String,
    pub primary_label: String,
    pub primary_label_loc: Span,
    pub secondary_labels: Vec<(String, Span)>,
    pub notes: Vec<String>,
}

#[derive(Default)]
struct Environment {
    variables: Vec<(String, Type)>,
    substitutions: Vec<Monotype>,
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

    fn pop_var(&mut self) -> Option<(String, Type)> {
        self.variables.pop()
    }

    fn new_var(&mut self) -> Monotype {
        let t = Monotype::TypeVar(self.substitutions.len());
        self.substitutions.push(t.clone());
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

        t
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
}

impl Type {
    fn instantiate(self, env: &mut Environment) -> Monotype {
        let mut replacements = HashMap::new();
        for generic in self.foralls.iter() {
            replacements.insert(generic.clone(), env.new_var());
        }

        self.monotype.instantiate(&replacements)
    }
}

impl Monotype {
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

        Ast::FuncCall { span, func, args } => todo!(),

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

        Ast::EmptyLet { span, symbol, type_, args } => todo!(),

        Ast::TopLet { span, symbol, args, value } => {
            typecheck_helper(value, env, errors)
        }

        Ast::If { span, cond, then, elsy } => todo!(),
        Ast::DatatypeDefinition { span, name, constraints, generics, variants } => todo!(),
        Ast::Match { span, value, patterns } => todo!(),
        Ast::Class { span, name, generics, constraints, functions } => todo!(),
        Ast::Instance { span, name, generics, parameters, constraints, functions } => todo!(),
    }
}

pub fn typecheck(asts: &mut [Ast]) -> Result<(), Vec<CheckError>> {
    let mut env = Environment::default().init_defaults();
    let mut errors = Vec::new();
    for ast in asts {
        let t = typecheck_helper(ast, &mut env, &mut errors);
        println!("{}: {}", ast, t);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
