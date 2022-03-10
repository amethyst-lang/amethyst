use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Range,
};

use super::parsing::Ast;

#[derive(Debug, Clone)]
pub struct Metadata<'a> {
    pub range: Range<usize>,
    pub type_: Type<'a>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

impl<'a> Type<'a> {
    pub fn has_generic(&self) -> bool {
        match self {
            Type::Tuple(v) => {
                for v in v {
                    if v.has_generic() {
                        return true;
                    }
                }
                false
            }

            Type::Pointer(_, v) => v.has_generic(),
            Type::Slice(_, v) => v.has_generic(),

            Type::Struct(_, v) => {
                for v in v {
                    if v.has_generic() {
                        return true;
                    }
                }
                false
            }

            Type::Generic(_) => true,
            Type::Function(a, r) => {
                for a in a {
                    if a.has_generic() {
                        return true;
                    }
                }
                r.has_generic()
            }
            _ => false,
        }
    }

    pub fn find_generics(&self, generics: &mut Vec<Self>) {
        match self {
            Type::Tuple(v) => {
                for v in v {
                    v.find_generics(generics);
                }
            }

            Type::Pointer(_, v) => v.find_generics(generics),
            Type::Slice(_, v) => v.find_generics(generics),

            Type::Struct(_, v) => {
                for v in v {
                    v.find_generics(generics);
                }
            }

            Type::Generic(_) => {
                if !generics.contains(self) {
                    generics.push(self.clone());
                }
            }

            Type::Function(a, r) => {
                for a in a {
                    a.find_generics(generics);
                }
                r.find_generics(generics)
            }

            _ => (),
        }
    }

    pub fn replace_generics(
        &mut self,
        type_var_counter: &mut u64,
        map: &mut HashMap<&'a str, Self>,
    ) {
        match self {
            Type::Tuple(v) => {
                for v in v {
                    v.replace_generics(type_var_counter, map);
                }
            }

            Type::Pointer(_, v) => v.replace_generics(type_var_counter, map),
            Type::Slice(_, v) => v.replace_generics(type_var_counter, map),

            Type::Struct(_, v) => {
                for v in v {
                    v.replace_generics(type_var_counter, map);
                }
            }

            Type::Generic(n) => match map.entry(*n) {
                Entry::Occupied(entry) => {
                    *self = entry.get().clone();
                }

                Entry::Vacant(entry) => {
                    *self = Type::TypeVariable(*type_var_counter);
                    *type_var_counter += 1;
                    entry.insert(self.clone());
                }
            },

            Type::Function(a, r) => {
                for a in a {
                    a.replace_generics(type_var_counter, map);
                }
                r.replace_generics(type_var_counter, map);
            }

            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LValue<'a> {
    Symbol(&'a str),
    Attribute(Box<LValue<'a>>, Vec<&'a str>),
    Deref(Box<LValue<'a>>),
    Get(Box<LValue<'a>>, Box<SExpr<'a>>),
}

#[derive(Debug, Clone)]
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
        elsy: Option<Box<SExpr<'a>>>,
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
        name: &'a str,
        values: Vec<(&'a str, SExpr<'a>)>,
    },

    Declare {
        meta: Metadata<'a>,
        mutable: bool,
        variable: &'a str,
        value: Box<SExpr<'a>>,
    },

    Assign {
        meta: Metadata<'a>,
        lvalue: LValue<'a>,
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
    InvalidLet,
    InvalidDefStruct,
    InvalidInst,
    InvalidLvalue,
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

        Ast::Symbol(_, v) => Ok(Type::Struct(v, vec![])),

        Ast::SExpr(_, v) if v.is_empty() => Ok(Type::Tuple(vec![])),

        Ast::Quote(_, ast) => match *ast {
            Ast::Symbol(_, v) => Ok(Type::Generic(v)),
            Ast::SExpr(_, v) => Ok(Type::Tuple(
                v.into_iter().map(parse_type).collect::<Result<_, _>>()?,
            )),
            _ => Err(LoweringError::InvalidType),
        },

        Ast::SExpr(_, mut ast) => match ast[0] {
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
                if ast.len() >= 2 {
                    let args = if let Ast::SExpr(_, v) = ast.remove(1) {
                        let mut args = vec![];
                        for v in v {
                            args.push(parse_type(v)?);
                        }
                        args
                    } else {
                        return Err(LoweringError::InvalidType);
                    };

                    if ast.len() == 3 {
                        if let Ast::Symbol(_, ":") = ast[1] {
                            Ok(Type::Function(args, Box::new(parse_type(ast.remove(2))?)))
                        } else {
                            Err(LoweringError::InvalidType)
                        }
                    } else if ast.len() == 1 {
                        Ok(Type::Function(args, Box::new(Type::Tuple(vec![]))))
                    } else {
                        Err(LoweringError::InvalidType)
                    }
                } else {
                    Err(LoweringError::InvalidType)
                }
            }

            Ast::Symbol(_, v) => Ok(Type::Struct(
                v,
                ast.into_iter()
                    .skip(1)
                    .map(parse_type)
                    .collect::<Result<_, _>>()?,
            )),

            _ => Err(LoweringError::InvalidType),
        },

        _ => Err(LoweringError::InvalidType),
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

        Ast::Char(range, value) => SExpr::Int {
            meta: Metadata {
                range,
                type_: Type::Int(false, 8),
            },
            value: value as u64,
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
                    values: sexpr
                        .into_iter()
                        .map(|v| lower_helper(v, false))
                        .collect::<Result<Vec<SExpr>, LoweringError>>()?,
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
                        values: sexpr
                            .into_iter()
                            .skip(1)
                            .map(|v| lower_helper(v, false))
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
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
                        values: sexpr
                            .into_iter()
                            .skip(1)
                            .map(|v| lower_helper(v, false))
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "seqn") => SExpr::Seq {
                        meta: Metadata {
                            range: range.clone(),
                            type_: Type::Unknown,
                        },
                        values: sexpr
                            .into_iter()
                            .skip(1)
                            .map(|v| lower_helper(v, false))
                            .chain([Ok(SExpr::Nil {
                                meta: Metadata {
                                    range,
                                    type_: Type::Tuple(vec![]),
                                },
                            })])
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "cond") => {
                        let elsy = if matches!(sexpr.last(), Some(Ast::SExpr(_, v)) if v.len() == 2 && matches!(v[0], Ast::Key(_, "else")))
                        {
                            if let Ast::SExpr(_, mut v) = sexpr.remove(sexpr.len() - 1) {
                                Some(Box::new(lower_helper(v.remove(1), false)?))
                            } else {
                                unreachable!()
                            }
                        } else {
                            None
                        };

                        SExpr::Cond {
                            meta: Metadata {
                                range,
                                type_: if elsy.is_none() {
                                    Type::Tuple(vec![])
                                } else {
                                    Type::Unknown
                                },
                            },

                            values: sexpr
                                .into_iter()
                                .skip(1)
                                .map(|v| match v {
                                    Ast::SExpr(_, mut v) if v.len() == 2 => Ok((
                                        lower_helper(v.swap_remove(0), false)?,
                                        lower_helper(v.remove(0), false)?,
                                    )),

                                    _ => Err(LoweringError::InvalidCondBranch),
                                })
                                .collect::<Result<_, LoweringError>>()?,
                            elsy,
                        }
                    }

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
                                    type_: parse_type(sexpr.swap_remove(1))?,
                                },
                                value: Box::new(lower_helper(sexpr.swap_remove(1), false)?),
                            }
                        } else {
                            return Err(LoweringError::InvalidTypeSpecifier);
                        }
                    }

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
                                type_: Type::Function(
                                    args.iter().map(|(_, v)| v.clone()).collect(),
                                    Box::new(ret_type.clone()),
                                ),
                            },
                            name,
                            ret_type,
                            args,
                            expr: Box::new(expr),
                        }
                    }

                    Ast::Symbol(_, "defstruct") => {
                        let name = match sexpr.remove(1) {
                            Ast::Symbol(_, name) => name,

                            _ => return Err(LoweringError::InvalidDefStruct),
                        };

                        let mut fields = vec![];
                        let mut generics = vec![];
                        for field in sexpr.into_iter().skip(1) {
                            match field {
                                Ast::SExpr(_, mut v) if v.len() == 2 => match v.swap_remove(0) {
                                    Ast::Symbol(_, name) => {
                                        let type_ = parse_type(v.swap_remove(0))?;
                                        type_.find_generics(&mut generics);
                                        fields.push((name, type_));
                                    }

                                    _ => return Err(LoweringError::InvalidDefStruct),
                                },

                                _ => return Err(LoweringError::InvalidDefStruct),
                            }
                        }

                        SExpr::StructDef {
                            meta: Metadata {
                                range,
                                type_: Type::Struct(name, generics),
                            },
                            name,
                            fields,
                        }
                    }

                    Ast::Symbol(_, "inst") => {
                        let name = match sexpr.remove(1) {
                            Ast::Symbol(_, name) => name,
                            _ => return Err(LoweringError::InvalidInst),
                        };

                        let mut values = vec![];
                        for field in sexpr.into_iter().skip(1) {
                            match field {
                                Ast::SExpr(_, mut v) if v.len() == 2 => match v.swap_remove(0) {
                                    Ast::Symbol(_, name) => {
                                        values.push((name, lower_helper(v.remove(0), false)?));
                                    }

                                    _ => return Err(LoweringError::InvalidInst),
                                },

                                _ => return Err(LoweringError::InvalidInst),
                            }
                        }

                        SExpr::StructSet {
                            meta: Metadata {
                                range,
                                type_: Type::Unknown,
                            },
                            name,
                            values,
                        }
                    }

                    Ast::Symbol(_, "let") => {
                        if sexpr.len() == 4 {
                            match (&sexpr[1], &sexpr[2]) {
                                (&Ast::Symbol(_, variable), &Ast::Symbol(_, "=")) => {
                                    SExpr::Declare {
                                        meta: Metadata {
                                            range,
                                            type_: Type::Unknown,
                                        },
                                        mutable: false,
                                        variable,
                                        value: Box::new(lower_helper(sexpr.remove(3), false)?),
                                    }
                                }

                                _ => return Err(LoweringError::InvalidLet),
                            }
                        } else if sexpr.len() == 5 {
                            match (&sexpr[1], &sexpr[2], &sexpr[3]) {
                                (
                                    &Ast::Symbol(_, "mut"),
                                    &Ast::Symbol(_, variable),
                                    &Ast::Symbol(_, "="),
                                ) => SExpr::Declare {
                                    meta: Metadata {
                                        range,
                                        type_: Type::Unknown,
                                    },
                                    mutable: true,
                                    variable,
                                    value: Box::new(lower_helper(sexpr.remove(4), false)?),
                                },

                                _ => return Err(LoweringError::InvalidLet),
                            }
                        } else {
                            return Err(LoweringError::InvalidLet);
                        }
                    }

                    Ast::Symbol(_, "set") => {
                        if sexpr.len() == 4 {
                            match &sexpr[2] {
                                Ast::Symbol(_, "=") => {
                                    let value = lower_helper(sexpr.swap_remove(3), false)?;
                                    let lvalue = lvalue_creator(sexpr.swap_remove(1))?;
                                    SExpr::Assign {
                                        meta: Metadata {
                                            range,
                                            type_: Type::Unknown,
                                        },
                                        lvalue,
                                        value: Box::new(value),
                                    }
                                }

                                _ => return Err(LoweringError::InvalidSet),
                            }
                        } else {
                            return Err(LoweringError::InvalidSet);
                        }
                    }

                    // Function call
                    _ => SExpr::FuncCall {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        func: Box::new(lower_helper(sexpr.remove(0), false)?),
                        values: sexpr
                            .into_iter()
                            .map(|v| lower_helper(v, false))
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },
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
                        if !attrs.is_empty() {
                            top = SExpr::Attribute {
                                meta: Metadata {
                                    range: range.start..end,
                                    type_: Type::Unknown,
                                },
                                top: Box::new(top),
                                attrs,
                            };

                            attrs = vec![];
                        }

                        let mut parent = lower_helper(attr, quoting)?;

                        match &mut parent {
                            SExpr::FuncCall { values, .. } => {
                                values.insert(0, top);
                                top = parent;
                            }

                            _ => return Err(LoweringError::InvalidAttribute),
                        }
                    }

                    _ => unreachable!("parser emits only symbols and s expressions"),
                }
            }

            if attrs.is_empty() {
                top
            } else {
                SExpr::Attribute {
                    meta: Metadata {
                        range,
                        type_: Type::Unknown,
                    },
                    top: Box::new(top),
                    attrs,
                }
            }
        }
    };

    Ok(sexpr)
}

fn lvalue_creator<'a>(ast: Ast<'a>) -> Result<LValue<'a>, LoweringError> {
    match ast {
        Ast::Symbol(_, sym) => Ok(LValue::Symbol(sym)),

        Ast::SExpr(_, mut v) => {
            let first = v.remove(0);
            match first {
                Ast::Symbol(_, "get") if v.len() == 2 => {
                    let index = lower_helper(v.remove(1), false)?;
                    let slice = lvalue_creator(v.remove(0))?;
                    Ok(LValue::Get(Box::new(slice), Box::new(index)))
                }

                Ast::Symbol(_, "deref") if v.len() == 1 => {
                    Ok(LValue::Deref(Box::new(lvalue_creator(v.remove(0))?)))
                }

                _ => return Err(LoweringError::InvalidLvalue),
            }
        }

        Ast::Attribute(_, mut v) => {
            let top = lvalue_creator(v.remove(0))?;
            let attrs = v
                .into_iter()
                .map(|v| match v {
                    Ast::Symbol(_, v) => Ok(v),
                    _ => Err(LoweringError::InvalidLvalue),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(LValue::Attribute(Box::new(top), attrs))
        }

        _ => Err(LoweringError::InvalidLvalue),
    }
}

pub fn lower(asts: Vec<Ast<'_>>) -> Result<Vec<SExpr<'_>>, LoweringError> {
    let mut sexprs = vec![];

    for ast in asts {
        sexprs.push(lower_helper(ast, false)?);
    }

    Ok(sexprs)
}
