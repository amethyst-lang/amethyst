use std::{collections::HashMap, ops::Range};

use super::parsing::Ast;

#[derive(Debug)]
pub struct Metadata<'a> {
    pub range: Range<usize>,
    pub type_: Type<'a>,
}

#[derive(Debug, PartialEq, Clone)]
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
    Slice(bool, Box<Type<'a>>),

    // name, generics
    Struct(&'a str, Vec<Type<'a>>),

    Generic(&'a str),

    // argument types, return type
    Function(Vec<Type<'a>>, Box<Type<'a>>),

    TypeVariable(u64),
}

#[derive(Debug)]
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

    Type {
        meta: Metadata<'a>,
        value: Box<SExpr<'a>>,
        type_: Type<'a>,
    },

    FuncDef {
        meta: Metadata<'a>,
        name: &'a str,
        ret_type: Type<'a>,
        args: Vec<(&'a str, Type<'a>)>,
        expr: Box<SExpr<'a>>,
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
            | SExpr::Quote { meta, .. }
            | SExpr::Comma { meta, .. }
            | SExpr::Backtick { meta, .. }
            | SExpr::Splice { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::Type { meta, .. }
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
            | SExpr::Quote { meta, .. }
            | SExpr::Comma { meta, .. }
            | SExpr::Backtick { meta, .. }
            | SExpr::Splice { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::Type { meta, .. }
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

#[derive(Debug)]
pub enum LoweringError {
    InvalidAttribute,
    TooManyQuoted,
    InvalidCondBranch,
    InvalidLoop,
    InvalidBreak,
    InvalidSet,
    InvalidTypeSpecifier,
    InvalidDefun,
    InvalidType,
}

fn parse_type(ast: Ast<'_>) -> Result<Type<'_>, LoweringError> {
    match ast {
        Ast::Symbol(_, "f32") => Ok(Type::F32),
        Ast::Symbol(_, "f64") => Ok(Type::F64),

        Ast::Symbol(_, int) if int.starts_with('i') || int.starts_with('u') => {
            if let Ok(v) = int[1..].parse() {
                Ok(Type::Int(int.starts_with('i'), v))
            } else {
                Err(LoweringError::InvalidType)
            }
        }

        Ast::Symbol(_, v) => {
            Ok(Type::Struct(v, vec![]))
        }

        Ast::SExpr(_, v) if v.is_empty() => {
            Ok(Type::Tuple(vec![]))
        }

        Ast::Quote(_, ast) => {
            match *ast {
                Ast::Symbol(_, v) => Ok(Type::Generic(v)),
                Ast::SExpr(_, v) => Ok(Type::Tuple(v.into_iter().map(parse_type).collect::<Result<_, _>>()?)),
                _ => Err(LoweringError::InvalidType),
            }
        }

        Ast::SExpr(_, mut ast) => {
            match ast[0] {
                Ast::Symbol(_, "*") => {
                    if ast.len() == 3 && matches!(ast[1], Ast::Symbol(_, "mut")) {
                        Ok(Type::Pointer(true, Box::new(parse_type(ast.remove(2))?)))
                    } else if ast.len() == 2 {
                        Ok(Type::Pointer(false, Box::new(parse_type(ast.remove(1))?)))
                    } else {
                        Err(LoweringError::InvalidType)
                    }
                }

                Ast::Symbol(_, "@") => {
                    if ast.len() == 3 && matches!(ast[1], Ast::Symbol(_, "mut")) {
                        Ok(Type::Slice(true, Box::new(parse_type(ast.remove(2))?)))
                    } else if ast.len() == 2 {
                        Ok(Type::Slice(false, Box::new(parse_type(ast.remove(1))?)))
                    } else {
                        Err(LoweringError::InvalidType)
                    }
                }

                Ast::Symbol(_, "fn") => {
                    todo!()
                }

                Ast::Symbol(_, v) => Ok(Type::Struct(v, ast.into_iter().skip(1).map(parse_type).collect::<Result<_, _>>()?)),

                _ => Err(LoweringError::InvalidType),
            }
        }

        _ => Err(LoweringError::InvalidType)
    }
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
                type_: Type::Slice(false, Box::new(Type::Int(false, 8))),
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
                    Ast::Symbol(_, "defmacro") => SExpr::Nil {
                        meta: Metadata {
                            range,
                            type_: Type::Tuple(vec![]),
                        },
                    },

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
                                    type_: Type::Tuple(vec![]),
                                },
                                value: Some(Box::new(lower_helper(sexpr.swap_remove(1), false)?)),
                            }
                        } else if sexpr.len() == 1 {
                            SExpr::Break {
                                meta: Metadata {
                                    range,
                                    type_: Type::Tuple(vec![]),
                                },
                                value: None,
                            }
                        } else {
                            return Err(LoweringError::InvalidBreak);
                        }
                    }

                    Ast::Symbol(_, ":") => {
                        if sexpr.len() == 3 {
                            SExpr::Type {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                value: Box::new(lower_helper(sexpr.swap_remove(1), false)?),
                                type_: parse_type(sexpr.swap_remove(1))?,
                            }
                        } else {
                            return Err(LoweringError::InvalidTypeSpecifier);
                        }
                    }

                    // (defun add (a i32) (b i32) : i32
                    //     (+ a b))
                    Ast::Symbol(_, "defun") => {
                        if sexpr.len() < 3 {
                            return Err(LoweringError::InvalidDefun);
                        }

                        let name = match sexpr[1] {
                            Ast::Symbol(_, name) => name,
                            _ => return Err(LoweringError::InvalidDefun),
                        };

                        let expr = lower_helper(sexpr.remove(sexpr.len() - 1), quoting)?;
                        let ret_type = if let Ast::Symbol(_, ":") = sexpr[sexpr.len() - 2] {
                            sexpr.remove(sexpr.len() - 2);
                            parse_type(sexpr.remove(sexpr.len() - 1))?
                        } else {
                            Type::Tuple(vec![])
                        };

                        let mut args = vec![];

                        for arg in sexpr.into_iter().skip(2) {
                            match arg {
                                Ast::SExpr(_, mut arg) if arg.len() == 2 => {
                                    if let Ast::Symbol(_, name) = arg[0] {
                                        args.push((name, parse_type(arg.remove(1))?));
                                    } else {
                                        return Err(LoweringError::InvalidDefun);
                                    }
                                }

                                _ => return Err(LoweringError::InvalidDefun),
                            }
                        }

                        SExpr::FuncDef {
                            meta: Metadata {
                                range,
                                type_: Type::Function(args.iter().map(|(_, v)| v.clone()).collect(), Box::new(ret_type.clone())),
                            },
                            name,
                            ret_type,
                            args,
                            expr: Box::new(expr),
                        }
                    }

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

                    // Function call
                    _ => {
                        SExpr::FuncCall {
                            meta: Metadata {
                                range,
                                type_: Type::Unknown,
                            },
                            func: Box::new(lower_helper(sexpr.remove(0), false)?),
                            values: sexpr.into_iter().map(|v| lower_helper(v, false)).collect::<Result<Vec<SExpr>, LoweringError>>()?,
                        }
                    }
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
