use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Type {
    Typevar(usize),
    Name(String),
    Generic(String),
    App(Box<Type>, Vec<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Typevar(i) => write!(f, "${}", i),
            Type::Name(n) => write!(f, "{}", n),
            Type::Generic(g) => write!(f, "'{}", g),

            Type::App(f_, a) => {
                write!(f, "{}[", f_)?;
                let mut first = true;
                for a in a {
                    if first {
                        write!(f, "{}", a)?;
                        first = false;
                    } else {
                        write!(f, ", {}", a)?;
                    }
                }

                write!(f, "]")
            }
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Integer(u64),
    Symbol(String),
    String(String),
    FuncCall {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Pattern {
    Wildcard,
    Symbol(String),
    Variant {
        name: String,
        args: Vec<Pattern>,
        exhaustive: bool,
    },
    Or(Vec<Pattern>),
}

#[derive(Debug)]
pub enum Statement {
    FuncCall {
        func: String,
        args: Vec<Expr>,
    },

    Let {
        name: String,
        value: Expr,
    },

    Set {
        name: String,
        value: Expr,
    },

    Loop {
        body: Vec<Statement>,
    },

    Break,
    Continue,

    Return(Option<Expr>),

    If {
        cond: Expr,
        then: Vec<Statement>,
        elsy: Vec<Statement>,
    },

    Match {
        value: Expr,
        branches: Vec<(Pattern, Vec<Statement>)>,
    },
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub result: Type,
}

#[derive(Debug)]
pub enum TopLevel {
    TypeDef {
        name: String,
        generics: Vec<String>,
        variants: Vec<Variant>,
    },

    FuncDef {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        stats: Vec<Statement>,
    },
}
