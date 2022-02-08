use std::{collections::HashMap, ops::Range};

use super::parsing::Ast;

pub struct Metadata<'a> {
    pub range: Range<usize>,
    pub type_: Type<'a>,
}

pub enum Type<'a> {
    Unknown,
    UnknownInt,
    UnknownFloat,

    // signed, width
    Int(bool, u8),

    F32,
    F64,

    Tuple(Vec<Type<'a>>),

    // mutable, type
    Pointer(bool, Box<Type<'a>>),

    // mutable, type
    FatPointer(bool, Box<Type<'a>>),

    // name, generics
    Struct(&'a str, Vec<Type<'a>>),

    Generic(&'a str),
}

pub enum SExpr<'a> {
    Int {
        meta: Metadata<'a>,
        value: u64,
    },
    Float {
        meta: Metadata<'a>,
        value: f64,
    },
    Str {
        meta: Metadata<'a>,
        value: String,
    },
    Symbol {
        meta: Metadata<'a>,
        value: &'a str,
    },
    List {
        meta: Metadata<'a>,
        values: Vec<SExpr<'a>>,
    },

    Key {
        meta: Metadata<'a>,
        value: &'a str,
    },

    Quote {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
    },

    Comma {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
    },

    Backtick {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
    },

    Splice {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
    },

    Seq {
        meta: Metadata<'a>,
        values: Vec<SExpr<'a>>,
    },

    Cond {
        meta: Metadata<'a>,
        values: Vec<(SExpr<'a>, SExpr<'a>)>,
    },

    Loop {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
    },

    Break {
        meta: Metadata<'a>,
        value: Option<Box<SExpr<'a>>>,
    },

    Nil {
        meta: Metadata<'a>,
    },

    FuncDef {
        meta: Metadata<'a>,
        name: &'a str,
        ret_type: Type<'a>,
        args: Vec<(&'a str, Type<'a>)>,
    },

    FuncCall {
        meta: Metadata<'a>,
        func: Box<SExpr<'a>>,
        values: Vec<SExpr<'a>>,
    },

    StructDef {
        meta: Metadata<'a>,
        name: &'a str,
        fields: Vec<(&'a str, Type<'a>)>,
    },

    StructSet {
        meta: Metadata<'a>,
        struct_name: &'a str,
        values: HashMap<&'a str, SExpr<'a>>,
    },

    Declare {
        meta: Metadata<'a>,
        mutable: bool,
        variable: &'a str,
        value: Box<SExpr<'a>>,
    },

    Assign {
        meta: Metadata<'a>,
        variable: &'a str,
        value: Box<SExpr<'a>>,
    },

    Attribute {
        meta: Metadata<'a>,
        top: Box<SExpr<'a>>,
        attrs: Vec<&'a str>,
    },
}

impl<'a> SExpr<'a> {
    pub fn meta(&self) -> &Metadata<'a> {
        match self {
            SExpr::Int { meta, .. }
            | SExpr::Float { meta, .. }
            | SExpr::Str { meta, .. }
            | SExpr::Symbol { meta, .. }
            | SExpr::List { meta, .. }
            | SExpr::Key { meta, .. }
            | SExpr::Quote { meta, .. }
            | SExpr::Comma { meta, .. }
            | SExpr::Backtick { meta, .. }
            | SExpr::Splice { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::FuncDef { meta, .. }
            | SExpr::FuncCall { meta, .. }
            | SExpr::StructDef { meta, .. }
            | SExpr::StructSet { meta, .. }
            | SExpr::Declare { meta, .. }
            | SExpr::Assign { meta, .. }
            | SExpr::Attribute { meta, .. } => meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut Metadata<'a> {
        match self {
            SExpr::Int { meta, .. }
            | SExpr::Float { meta, .. }
            | SExpr::Str { meta, .. }
            | SExpr::Symbol { meta, .. }
            | SExpr::List { meta, .. }
            | SExpr::Key { meta, .. }
            | SExpr::Quote { meta, .. }
            | SExpr::Comma { meta, .. }
            | SExpr::Backtick { meta, .. }
            | SExpr::Splice { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::FuncDef { meta, .. }
            | SExpr::FuncCall { meta, .. }
            | SExpr::StructDef { meta, .. }
            | SExpr::StructSet { meta, .. }
            | SExpr::Declare { meta, .. }
            | SExpr::Assign { meta, .. }
            | SExpr::Attribute { meta, .. } => meta,
        }
    }
}

pub enum LoweringError {
    InvalidAttribute,
    TooManyQuoted,
    InvalidCondBranch,
    InvalidLoop,
    InvalidBreak,
    InvalidSet,
}

fn lower_helper(ast: Ast<'_>, quoting: bool) -> Result<SExpr<'_>, LoweringError> {
    let sexpr = match ast {
        Ast::Int(range, value) => SExpr::Int {
            meta: Metadata {
                range,
                type_: Type::UnknownInt,
            },
            value,
        },

        Ast::Float(range, value) => SExpr::Float {
            meta: Metadata {
                range,
                type_: Type::UnknownFloat,
            },
            value,
        },

        Ast::Str(range, value) => SExpr::Str {
            meta: Metadata {
                range,
                type_: Type::FatPointer(false, Box::new(Type::Int(false, 8))),
            },
            value,
        },

        Ast::Symbol(range, value) => SExpr::Symbol {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value,
        },

        Ast::Key(_, _) => todo!(),

        Ast::Quote(range, value) => SExpr::Quote {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value: Box::new(lower_helper(*value, true)?),
        },

        Ast::Comma(range, value) => SExpr::Comma {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value: Box::new(lower_helper(*value, quoting)?),
        },

        Ast::Backtick(range, value) => SExpr::Backtick {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value: Box::new(lower_helper(*value, quoting)?),
        },

        Ast::Splice(range, value) => SExpr::Splice {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value: Box::new(lower_helper(*value, quoting)?),
        },

        Ast::SExpr(range, mut sexpr) => {
            if sexpr.is_empty() {
                SExpr::Nil {
                    meta: Metadata {
                        range,
                        type_: Type::Tuple(vec![]),
                    },
                }
            } else if quoting {
                SExpr::List {
                    meta: Metadata {
                        range,
                        type_: Type::Unknown,
                    },
                    values: sexpr.into_iter().map(|v| lower_helper(v, false)).collect::<Result<Vec<SExpr>, LoweringError>>()?,
                }
            } else {
                match &sexpr[0] {
                    Ast::Symbol(_, "list") => SExpr::List {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        values: sexpr.into_iter().skip(1).map(|v| lower_helper(v, false)).collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "quote") => {
                        if sexpr.len() == 2 {
                            SExpr::Quote {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                value: Box::new(lower_helper(sexpr.swap_remove(2), true)?),
                            }
                        } else {
                            return Err(LoweringError::TooManyQuoted);
                        }
                    }

                    Ast::Symbol(_, "seq") => SExpr::Seq {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        values: sexpr.into_iter().skip(1).map(|v| lower_helper(v, false)).collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "cond") => SExpr::Cond {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },

                        values: sexpr.into_iter().skip(1).map(|v| {
                            match v {
                                Ast::SExpr(_, mut v) if v.len() == 2 => {
                                    Ok((lower_helper(v.swap_remove(0), false)?, lower_helper(v.remove(0), false)?))
                                }

                                _ => Err(LoweringError::InvalidCondBranch)
                            }
                        }).collect::<Result<_, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "loop") => {
                        if sexpr.len() == 2 {
                            SExpr::Loop {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                value: Box::new(lower_helper(sexpr.swap_remove(1), false)?),
                            }
                        } else {
                            return Err(LoweringError::InvalidLoop);
                        }
                    }

                    Ast::Symbol(_, "break") => {
                        if sexpr.len() == 2 {
                            SExpr::Break {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                value: Some(Box::new(lower_helper(sexpr.swap_remove(1), false)?)),
                            }
                        } else if sexpr.len() == 1 {
                            SExpr::Break {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                value: None,
                            }
                        } else {
                            return Err(LoweringError::InvalidBreak);
                        }
                    }

                    Ast::Symbol(_, "defun") => todo!(),
                    Ast::Symbol(_, "defstruct") => todo!(),
                    Ast::Symbol(_, "inst") => todo!(),

                    Ast::Symbol(_, "let" | "mut") => todo!(),

                    Ast::Symbol(_, "set") => {
                        if sexpr.len() == 3 {
                            match sexpr[1] {
                                Ast::Symbol(_, variable) => SExpr::Assign {
                                    meta: Metadata {
                                        range,
                                        type_: Type::Unknown,
                                    },
                                    variable,
                                    value: Box::new(lower_helper(sexpr.swap_remove(2), false)?),
                                },

                                _ => return Err(LoweringError::InvalidSet),
                            }
                        } else {
                            return Err(LoweringError::InvalidSet);
                        }
                    }

                    Ast::Key(_, _) => todo!(),
                    Ast::Quote(_, _) => todo!(),
                    Ast::Comma(_, _) => todo!(),
                    Ast::Backtick(_, _) => todo!(),
                    Ast::Splice(_, _) => todo!(),
                    Ast::SExpr(_, _) => todo!(),
                    _ => todo!(),
                }
            }
        }

        Ast::Attribute(range, mut attrs_raw) => {
            let top = attrs_raw.remove(0);
            let mut top = lower_helper(top, quoting)?;

            let mut attrs = vec![];
            let mut end;

            for attr in attrs_raw {
                end = attr.span().end;
                match attr {
                    Ast::Symbol(_, v) => attrs.push(v),

                    Ast::SExpr(..) => {
                        top = SExpr::Attribute {
                            meta: Metadata {
                                range: range.start..end,
                                type_: Type::Unknown,
                            },
                            top: Box::new(top),
                            attrs,
                        };

                        let mut parent = lower_helper(attr, quoting)?;

                        match &mut parent {
                            SExpr::FuncCall { values, .. } => {
                                values.insert(0, top);
                                top = parent;
                            }

                            _ => return Err(LoweringError::InvalidAttribute)
                        }

                        attrs = vec![];
                    }

                    _ => unreachable!("parser emits only symbols and s expressions"),
                }
            }

            SExpr::Attribute {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                top: Box::new(top),
                attrs,
            }
        }
    };

    Ok(sexpr)
}

pub fn lower(asts: Vec<Ast<'_>>) -> Result<Vec<SExpr<'_>>, LoweringError> {
    let mut sexprs = vec![];

    for ast in asts {
        sexprs.push(lower_helper(ast, false)?);
    }

    Ok(sexprs)
}
