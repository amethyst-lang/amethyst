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

    fn find_var(&mut self, var: &str) -> Option<Monotype> {
        for (key, type_) in self.variables.iter().rev() {
            if key == var {
                return Some(type_.clone().instantiate(self));
            }
        }

        return None;
    }

    fn new_var(&mut self) -> Monotype {
        let t = Monotype::TypeVar(self.substitutions.len());
        self.substitutions.push(t.clone());
        t
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
        Ast::Symbol(_, _) => todo!(),

        Ast::Binary { span, op, left, right } => {
            if let Some(op_type) = env.find_var(op) {
                let t_left = typecheck_helper(left, env, errors);
                let t_right = typecheck_helper(right, env, errors);
                todo!()
            } else {
                Monotype::Base(BaseType::Bottom)
            }
        }

        Ast::FuncCall { span, func, args } => todo!(),
        Ast::Let { span, mutable, symbol, args, value, context } => todo!(),
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
