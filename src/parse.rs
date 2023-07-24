pub enum Type {
    Typevar(usize),
    Name(String),
    Generic(String),
    App(Box<Type>, Vec<Type>),
}

pub enum Expr {
    Integer(usize),
    Bool(bool),
    Symbol(String),

    FuncCall {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

pub enum Pattern {
    Wildcard,
    Symbol(String),
    Variant(String, Vec<Pattern>),
    Or(Vec<Pattern>),
}

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

    If {
        cond: Expr,
        then: Vec<Statement>,
        elsy: Vec<Statement>,
    },

    Match {
        value: Expr,
        branches: Vec<(Pattern, Vec<Statement>)>,
    },

    While {
        cond: Expr,
        body: Vec<Statement>,
    },
}

pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub result: Type,
}

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
