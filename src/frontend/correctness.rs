use std::{ops::Range, collections::HashMap};

pub struct Metadata {
    pub range: Range<usize>,
}

pub enum Type<'a> {
    I(u8),
    U(u8),
    F32,
    F64,
    Pointer(bool, Box<Type<'a>>),
    FatPointer(bool, Box<Type<'a>>),
    Struct(&'a str),
    Generic(&'a str),
}

pub enum SExpr<'a> {
    Int {
        meta: Metadata,
        value: u64,
    },
    Float {
        meta: Metadata,
        value: f64,
    },
    Str {
        meta: Metadata,
        value: String,
    },
    Symbol {
        meta: Metadata,
        value: &'a str,
    },

    Key {
        meta: Metadata,
        value: &'a str,
    },

    Quote {
        meta: Metadata,
        value: Box<SExpr<'a>>,
    },

    Comma {
        meta: Metadata,
        value: Box<SExpr<'a>>,
    },

    Backtick {
        meta: Metadata,
        value: Box<SExpr<'a>>,
    },

    Splice {
        meta: Metadata,
        value: Box<SExpr<'a>>,
    },

    Seq {
        meta: Metadata,
        values: Vec<SExpr<'a>>,
    },

    Cond {
        meta: Metadata,
        values: Vec<(SExpr<'a>, SExpr<'a>)>,
        elsy: Option<Box<SExpr<'a>>>,
    },

    Loop {
        meta: Metadata,
        value: Box<SExpr<'a>>,
    },

    Nil {
        meta: Metadata,
    },

    FuncDef {
        meta: Metadata,
        name: &'a str,
        ret_type: Type<'a>,
        args: Vec<(&'a str, Type<'a>)>,
    },

    FuncCall {
        meta: Metadata,
        func: Box<SExpr<'a>>,
        values: Vec<SExpr<'a>>,
    },

    StructDef {
        meta: Metadata,
        name: &'a str,
        fields: Vec<(&'a str, Type<'a>)>,
    },

    StructSet {
        meta: Metadata,
        struct_name: &'a str,
        values: HashMap<&'a str, SExpr<'a>>,
    },

    Declare {
        meta: Metadata,
        mutable: bool,
        variable: &'a str,
        value: Box<SExpr<'a>>,
    },

    Assign {
        meta: Metadata,
        variable: &'a str,
        value: Box<SExpr<'a>>,
    },
}
