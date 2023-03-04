use crate::parse::{Ast, Type, BaseType};

#[derive(Debug, Default)]
struct Environment {
    variables: Vec<(String, Type)>,
    substitutions: Vec<Type>,
}

impl Environment {
    fn new_type_var(&mut self) -> Type {
        let t = Type::TypeVar(self.substitutions.len());
        self.substitutions.push(t.clone());
        t
    }

    fn push_variable(&mut self, var: &str, type_: &Type) {
        self.variables.push((var.to_string(), type_.clone()));
    }

    fn find_variable(&mut self, var: &str) -> Option<&Type> {
        self.variables.iter().rev().find(|(v, _)| v == var).map(|(_, t)| t)
    }
}

impl Type {
    fn equals_up_to_env(&mut self, other: &mut Type, env: &mut Environment) -> bool {
        match (self, other) {
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
            (Type::Base(BaseType::Named(n1, p1, _)), Type::Base(BaseType::Named(n2, p2, _))) => n1 == n2 && p1.iter_mut().zip(p2.iter_mut()).all(|(p1, p2)| p1.equals_up_to_env(p2, env)),
            (Type::Func(a1, r1), Type::Func(a2, r2)) => a1.equals_up_to_env(a2, env) && r1.equals_up_to_env(r2, env),

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
                    if matches!(a, Type::TypeVar(y) if x == *y) {
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
            Type::Base(BaseType::Named(_, params, _)) => params.iter_mut().all(|v| v.replace_type_vars(env)),

            Type::Base(_) => true,

            Type::Func(a, r) => a.replace_type_vars(env) && r.replace_type_vars(env),

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

                !matches!(self, Type::TypeVar(_))
            }
        }
    }
}

fn replace_unknowns(env: &mut Environment, ast: &mut Ast) {
    match ast {
        Ast::Binary { left, right, .. } => {
            replace_unknowns(env, left);
            replace_unknowns(env, right);
        }

        Ast::FuncCall { func, args } => {
            replace_unknowns(env, func);
            for arg in args {
                replace_unknowns(env, arg);
            }
        }

        Ast::Let { args, ret_type, value, context, .. } => {
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

        Ast::TopLet { args, ret_type, value, .. } => {
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

        Ast::If { cond, then, elsy } => {
            replace_unknowns(env, cond);
            replace_unknowns(env, then);
            replace_unknowns(env, elsy);
        }

        _ => (),
    }
}

fn typecheck_helper(env: &mut Environment, ast: &mut Ast) -> Result<Type, ()> {
    match ast {
        Ast::Integer(_) => Ok(Type::Base(BaseType::I32)),
        Ast::Bool(_) => Ok(Type::Base(BaseType::Bool)),

        Ast::Symbol(s) => env.find_variable(s).cloned().ok_or(()),

        Ast::Binary { .. } => todo!(),

        Ast::FuncCall { .. } => todo!(),

        Ast::Let { .. } => todo!(),

        Ast::TopLet { symbol, args, ret_type, value, .. } => {
            let mut t = typecheck_helper(env, value)?;
            if !ret_type.equals_up_to_env(&mut t, env) {
                Err(())
            } else {
                Ok(t) // TODO: actual type
            }
        }

        Ast::If { cond, then, elsy } => {
            let mut cond = typecheck_helper(env, cond)?;
            let mut then = typecheck_helper(env, then)?;
            let mut elsy = typecheck_helper(env, elsy)?;

            if !cond.equals_up_to_env(&mut Type::Base(BaseType::Bool), env) {
                return Err(());
            }

            if then.equals_up_to_env(&mut elsy, env) {
                Ok(then)
            } else {
                Err(())
            }
        }
    }
}

fn replace_type_vars(ast: &mut Ast, env: &Environment) -> Result<(), ()> {
    match ast {
        Ast::Binary { left, right, .. } => {
            replace_type_vars(left, env)?;
            replace_type_vars(right, env)
        }

        Ast::FuncCall { func, args } => {
            replace_type_vars(func, env)?;
            for arg in args {
                replace_type_vars(arg, env)?;
            }
            Ok(())
        }

        Ast::Let { args, ret_type, value, context, .. } => {
            for (_, arg_type) in args.iter_mut() {
                if !arg_type.replace_type_vars(env) {
                    return Err(());
                }
            }
            if !ret_type.replace_type_vars(env) {
                return Err(());
            }

            replace_type_vars(value, env)?;
            replace_type_vars(context, env)
        }

        Ast::TopLet { args, ret_type, value, .. } => {
            for (_, arg_type) in args.iter_mut() {
                if !arg_type.replace_type_vars(env) {
                    return Err(());
                }
            }
            if !ret_type.replace_type_vars(env) {
                return Err(());
            }

            replace_type_vars(value, env)
        }

        Ast::If { cond, then, elsy } => {
            replace_type_vars(cond, env)?;
            replace_type_vars(then, env)?;
            replace_type_vars(elsy, env)
        }

        _ => Ok(()),
    }
}

pub fn typecheck(asts: &mut [Ast]) -> Result<(), ()> {
    let mut env = Environment::default();

    for ast in asts.iter_mut() {
        replace_unknowns(&mut env, ast);
    }

    for ast in asts.iter() {
        if let Ast::TopLet { symbol, args, ret_type, .. } = ast {
            let mut top = ret_type.clone();
            for (_, arg_type) in args.iter().rev() {
                top = Type::Func(Box::new(arg_type.clone()), Box::new(top));
            }
            env.push_variable(symbol, &top);
        }
    }

    for ast in asts.iter_mut() {
        typecheck_helper(&mut env, ast)?;
    }

    for ast in asts.iter_mut() {
        replace_type_vars(ast, &env)?;
    }

    Ok(())
}