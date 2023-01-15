use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Range,
};

use super::parsing::Ast;

#[derive(Debug, Clone)]
pub struct Metadata {
    pub range: Range<usize>,
    pub type_: Type,
}

#[derive(Debug, Clone, Default)]
pub struct Annotations {
    pub public: bool,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    Unknown,
    UnknownInt,
    UnknownFloat,

    // signed, width
    Int(bool, u8),

    F32,
    F64,

    Tuple(Vec<Type>),

    // mutable, type
    Pointer(bool, Box<Type>),

    // mutable, type
    Slice(bool, Box<Type>),

    // name, generics
    Struct(String, Vec<Type>),

    Union(Vec<Type>),

    Generic(String),

    // argument types, return type
    Function(Vec<Type>, Box<Type>),

    TypeVariable(u64),
}

impl Type {
    pub fn is_subtype(&self, supertype: &Type) -> bool {
        match (self, supertype) {
            (Type::Int(s1, w1), Type::Int(s2, w2)) => s1 == s2 && w1 == w2,
            (Type::F32, Type::F32) | (Type::F64, Type::F64) => true,
            (Type::Tuple(v1), Type::Tuple(v2)) => {
                v1.len() == v2.len() && v1.iter().zip(v2.iter()).all(|(v1, v2)| v1.is_subtype(v2))
            }
            (Type::Pointer(_, v1), Type::Pointer(_, v2))
            | (Type::Slice(_, v1), Type::Slice(_, v2)) => v1.is_subtype(v2),
            (Type::Function(a1, r1), Type::Function(a2, r2)) => {
                r1.is_subtype(r2)
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2.iter()).all(|(v1, v2)| v1.is_subtype(v2))
            }

            (Type::Union(v1), Type::Union(v2)) => {
                let mut is_subset = true;
                for a in v1.iter() {
                    let mut not_found = true;
                    for b in v2.iter() {
                        if a == b {
                            not_found = false;
                            break;
                        }
                    }

                    if not_found {
                        is_subset = false;
                        break;
                    }
                }

                if is_subset {
                    true
                } else {
                    let mut is_subset = true;
                    for a in v2.iter() {
                        let mut not_found = true;
                        for b in v1.iter() {
                            if a == b {
                                not_found = false;
                                break;
                            }
                        }

                        if not_found {
                            is_subset = false;
                            break;
                        }
                    }

                    is_subset
                }
            }

            (Type::Union(v), x) | (x, Type::Union(v)) => v.iter().any(|v| v.is_subtype(x)),

            _ => false,
        }
    }

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

            Type::Struct(_, v) | Type::Union(v) => {
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

            Type::Struct(_, v) | Type::Union(v) => {
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
        map: &mut HashMap<String, Self>,
    ) {
        match self {
            Type::Tuple(v) => {
                for v in v {
                    v.replace_generics(type_var_counter, map);
                }
            }

            Type::Pointer(_, v) => v.replace_generics(type_var_counter, map),
            Type::Slice(_, v) => v.replace_generics(type_var_counter, map),

            Type::Struct(_, v) | Type::Union(v) => {
                for v in v {
                    v.replace_generics(type_var_counter, map);
                }
            }

            Type::Generic(n) => match map.entry(n.clone()) {
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
pub enum SExpr {
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
        value: String,
    },

    Tuple {
        meta: Metadata,
        tuple: Vec<SExpr>,
    },

    Seq {
        meta: Metadata,
        values: Vec<SExpr>,
    },

    Cond {
        meta: Metadata,
        values: Vec<(SExpr, SExpr)>,
        elsy: Option<Box<SExpr>>,
    },

    Loop {
        meta: Metadata,
        value: Box<SExpr>,
    },

    Break {
        meta: Metadata,
        value: Option<Box<SExpr>>,
    },

    Nil {
        meta: Metadata,
    },

    Type {
        meta: Metadata,
        value: Box<SExpr>,
    },

    FuncDef {
        meta: Metadata,
        ann: Annotations,
        name: String,
        ret_type: Type,
        args: Vec<(String, Type)>,
        expr: Box<SExpr>,
    },

    FuncCall {
        meta: Metadata,
        func: Box<SExpr>,
        values: Vec<SExpr>,
    },

    FuncExtern {
        meta: Metadata,
        name: String,
        ret_type: Type,
        args: Vec<(String, Type)>,
        linked_to: String,
    },

    StructDef {
        meta: Metadata,
        ann: Annotations,
        name: String,
        fields: Vec<(String, Type)>,
    },

    StructSet {
        meta: Metadata,
        name: String,
        values: Vec<(String, SExpr)>,
    },

    Declare {
        meta: Metadata,
        conditional: bool,
        settings: Vec<SExpr>,
    },

    Assign {
        meta: Metadata,
        var: String,
        value: Box<SExpr>,
    },

    Attribute {
        meta: Metadata,
        top: Box<SExpr>,
        attr: String,
    },

    SliceGet {
        meta: Metadata,
        top: Box<SExpr>,
        index: Box<SExpr>,
    },

    SizeOf {
        meta: Metadata,
        type_: Type,
    },

    Ref {
        meta: Metadata,
        value: Box<SExpr>,
    },

    Deref {
        meta: Metadata,
        value: Box<SExpr>,
    },

    Import {
        meta: Metadata,
        imports: Vec<(Option<String>, String)>,
    },
}

impl SExpr {
    pub fn meta(&self) -> &Metadata {
        match self {
            SExpr::Int { meta, .. }
            | SExpr::Float { meta, .. }
            | SExpr::Str { meta, .. }
            | SExpr::Symbol { meta, .. }
            | SExpr::Tuple { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::Type { meta, .. }
            | SExpr::FuncDef { meta, .. }
            | SExpr::FuncCall { meta, .. }
            | SExpr::FuncExtern { meta, .. }
            | SExpr::StructDef { meta, .. }
            | SExpr::StructSet { meta, .. }
            | SExpr::Declare { meta, .. }
            | SExpr::Assign { meta, .. }
            | SExpr::Attribute { meta, .. }
            | SExpr::SliceGet { meta, .. }
            | SExpr::SizeOf { meta, .. }
            | SExpr::Ref { meta, .. }
            | SExpr::Deref { meta, .. }
            | SExpr::Import { meta, .. } => meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut Metadata {
        match self {
            SExpr::Int { meta, .. }
            | SExpr::Float { meta, .. }
            | SExpr::Str { meta, .. }
            | SExpr::Symbol { meta, .. }
            | SExpr::Tuple { meta, .. }
            | SExpr::Seq { meta, .. }
            | SExpr::Cond { meta, .. }
            | SExpr::Loop { meta, .. }
            | SExpr::Break { meta, .. }
            | SExpr::Nil { meta }
            | SExpr::Type { meta, .. }
            | SExpr::FuncDef { meta, .. }
            | SExpr::FuncCall { meta, .. }
            | SExpr::FuncExtern { meta, .. }
            | SExpr::StructDef { meta, .. }
            | SExpr::StructSet { meta, .. }
            | SExpr::Declare { meta, .. }
            | SExpr::Assign { meta, .. }
            | SExpr::Attribute { meta, .. }
            | SExpr::SliceGet { meta, .. }
            | SExpr::SizeOf { meta, .. }
            | SExpr::Ref { meta, .. }
            | SExpr::Deref { meta, .. }
            | SExpr::Import { meta, .. } => meta,
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
    InvalidDefun(String),
    InvalidType,
    InvalidLet,
    InvalidDefStruct,
    InvalidInst,
    InvalidLvalue,
    InvalidSizeOf,
    InvalidImport,
}

fn parse_type(ast: Ast<'_>) -> Result<Type, LoweringError> {
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

        Ast::Symbol(_, v) => Ok(Type::Struct(v.to_string(), vec![])),

        Ast::SExpr(_, v) if v.is_empty() => Ok(Type::Tuple(vec![])),

        Ast::Quote(_, ast) => match *ast {
            Ast::Symbol(_, v) => Ok(Type::Generic(v.to_string())),
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

            Ast::Symbol(_, "union") => Ok(Type::Union(
                ast.into_iter()
                    .skip(1)
                    .map(parse_type)
                    .collect::<Result<_, _>>()?,
            )),

            Ast::Symbol(_, "tuple") => Ok(Type::Tuple(
                ast.into_iter()
                    .skip(1)
                    .map(parse_type)
                    .collect::<Result<_, _>>()?,
            )),

            Ast::Symbol(_, v) => Ok(Type::Struct(
                v.to_string(),
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

fn lower_helper(ast: Ast<'_>, ann: Annotations) -> Result<SExpr, LoweringError> {
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
            value: value.to_string(),
        },

        Ast::SymbolOwned(range, value) => SExpr::Symbol {
            meta: Metadata {
                range,
                type_: Type::Unknown,
            },
            value,
        },

        Ast::Key(span, key) => todo!("{:?}: {}", span, key),

        Ast::Quote(_, _) => todo!(),

        Ast::SExpr(range, mut sexpr) => {
            if sexpr.is_empty() {
                SExpr::Nil {
                    meta: Metadata {
                        range,
                        type_: Type::Tuple(vec![]),
                    },
                }
            } else {
                match &sexpr[0] {
                    Ast::Symbol(_, "defmacro") => SExpr::Nil {
                        meta: Metadata {
                            range,
                            type_: Type::Tuple(vec![]),
                        },
                    },

                    Ast::Symbol(_, "import") => {
                        let mut tag = None;
                        let mut imports = Vec::new();

                        for ast in sexpr.into_iter().skip(1) {
                            match ast {
                                Ast::Str(_, s) => {
                                    imports.push((tag, s));
                                    tag = None;
                                }

                                Ast::Key(_, s) if tag.is_none() => tag = Some(s.to_string()),

                                _ => return Err(LoweringError::InvalidImport),
                            }
                        }

                        SExpr::Import {
                            meta: Metadata {
                                range,
                                type_: Type::Tuple(vec![]),
                            },
                            imports,
                        }
                    }

                    Ast::Symbol(_, "tuple") => SExpr::Tuple {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        tuple: sexpr
                            .into_iter()
                            .skip(1)
                            .map(|v| lower_helper(v, Annotations::default()))
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },

                    Ast::Symbol(_, "seq") => SExpr::Seq {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        values: sexpr
                            .into_iter()
                            .skip(1)
                            .map(|v| lower_helper(v, Annotations::default()))
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
                            .map(|v| lower_helper(v, Annotations::default()))
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
                                Some(Box::new(lower_helper(v.remove(1), Annotations::default())?))
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
                                        lower_helper(v.swap_remove(0), Annotations::default())?,
                                        lower_helper(v.remove(0), Annotations::default())?,
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
                                value: Box::new(lower_helper(
                                    sexpr.swap_remove(1),
                                    Annotations::default(),
                                )?),
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
                                value: Some(Box::new(lower_helper(
                                    sexpr.swap_remove(1),
                                    Annotations::default(),
                                )?)),
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
                                    type_: parse_type(sexpr.swap_remove(2))?,
                                },
                                value: Box::new(lower_helper(
                                    sexpr.swap_remove(1),
                                    Annotations::default(),
                                )?),
                            }
                        } else {
                            return Err(LoweringError::InvalidTypeSpecifier);
                        }
                    }

                    Ast::Symbol(_, "defun") => {
                        if sexpr.len() < 3 {
                            return Err(LoweringError::InvalidDefun(String::from("unknown")));
                        }

                        let name = match sexpr[1] {
                            Ast::Symbol(_, name) => name,
                            _ => return Err(LoweringError::InvalidDefun(String::from("unknown"))),
                        };

                        let expr =
                            lower_helper(sexpr.remove(sexpr.len() - 1), Annotations::default())?;
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
                                        args.push((name.to_string(), parse_type(arg.remove(1))?));
                                    } else {
                                        return Err(LoweringError::InvalidDefun(String::from(
                                            name,
                                        )));
                                    }
                                }

                                _ => return Err(LoweringError::InvalidDefun(String::from(name))),
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
                            ann,
                            name: name.to_string(),
                            ret_type,
                            args,
                            expr: Box::new(expr),
                        }
                    }

                    Ast::Symbol(_, "defext") => {
                        if sexpr.len() < 3 {
                            return Err(LoweringError::InvalidDefun(String::from("unknown")));
                        }

                        let name = match sexpr[1] {
                            Ast::Symbol(_, name) => name,
                            _ => return Err(LoweringError::InvalidDefun(String::from("unknown"))),
                        };

                        let linked_to = match sexpr.remove(sexpr.len() - 1) {
                            Ast::Str(_, linked) => linked,
                            _ => return Err(LoweringError::InvalidDefun(String::from(name))),
                        };

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
                                        args.push((name.to_string(), parse_type(arg.remove(1))?));
                                    } else {
                                        return Err(LoweringError::InvalidDefun(String::from(
                                            name,
                                        )));
                                    }
                                }

                                _ => return Err(LoweringError::InvalidDefun(String::from(name))),
                            }
                        }

                        SExpr::FuncExtern {
                            meta: Metadata {
                                range,
                                type_: Type::Function(
                                    args.iter().map(|(_, v)| v.clone()).collect(),
                                    Box::new(ret_type.clone()),
                                ),
                            },
                            name: name.to_string(),
                            ret_type,
                            args,
                            linked_to,
                        }
                    }

                    Ast::Symbol(_, "defstruct") => {
                        let name = match sexpr.remove(1) {
                            Ast::Symbol(_, name) => name,

                            _ => return Err(LoweringError::InvalidDefStruct),
                        };

                        let mut fields = vec![];
                        let mut generics = vec![];
                        let mut current_field_name = None;
                        for field in sexpr.into_iter().skip(1) {
                            match current_field_name {
                                Some(name) => {
                                    let type_ = parse_type(field)?;
                                    type_.find_generics(&mut generics);
                                    fields.push((name, type_));
                                    current_field_name = None;
                                }

                                None => match field {
                                    Ast::Key(_, name) => {
                                        current_field_name = Some(name.to_string());
                                    }

                                    _ => return Err(LoweringError::InvalidDefStruct),
                                },
                            }
                        }

                        if current_field_name.is_some() {
                            return Err(LoweringError::InvalidDefStruct);
                        }

                        SExpr::StructDef {
                            meta: Metadata {
                                range,
                                type_: Type::Struct(name.to_string(), generics),
                            },
                            ann,
                            name: name.to_string(),
                            fields,
                        }
                    }

                    Ast::Symbol(_, "inst") => {
                        let name = match sexpr.remove(1) {
                            Ast::Symbol(_, name) => name,
                            _ => return Err(LoweringError::InvalidInst),
                        };

                        let mut values = vec![];
                        let mut current_field_name = None;
                        for field in sexpr.into_iter().skip(1) {
                            match current_field_name {
                                Some(name) => {
                                    values
                                        .push((name, lower_helper(field, Annotations::default())?));
                                    current_field_name = None;
                                }

                                None => match field {
                                    Ast::Key(_, name) => {
                                        current_field_name = Some(name.to_string());
                                    }

                                    _ => return Err(LoweringError::InvalidInst),
                                },
                            }
                        }

                        if current_field_name.is_some() {
                            return Err(LoweringError::InvalidInst);
                        }

                        SExpr::StructSet {
                            meta: Metadata {
                                range,
                                type_: Type::Unknown,
                            },
                            name: name.to_string(),
                            values,
                        }
                    }

                    Ast::Symbol(_, l @ ("let" | "let?")) => {
                        if sexpr.len() > 1 {
                            SExpr::Declare {
                                meta: Metadata {
                                    range,
                                    type_: Type::Unknown,
                                },
                                conditional: l.ends_with('?'),
                                settings: sexpr
                                    .into_iter()
                                    .skip(1)
                                    .map(|v| lower_helper(v, Annotations::default()))
                                    .collect::<Result<_, _>>()?,
                            }
                        } else {
                            return Err(LoweringError::InvalidLet);
                        }
                    }

                    Ast::Symbol(_, "=") => {
                        if sexpr.len() == 3 {
                            let value = lower_helper(sexpr.swap_remove(2), Annotations::default())?;
                            match sexpr.swap_remove(1) {
                                Ast::Symbol(_, var) => SExpr::Assign {
                                    meta: Metadata {
                                        range,
                                        type_: Type::Unknown,
                                    },
                                    var: var.to_string(),
                                    value: Box::new(value),
                                },

                                Ast::SymbolOwned(_, var) => SExpr::Assign {
                                    meta: Metadata {
                                        range,
                                        type_: Type::Unknown,
                                    },
                                    var,
                                    value: Box::new(value),
                                },

                                _ => return Err(LoweringError::InvalidSet),
                            }
                        } else {
                            return Err(LoweringError::InvalidSet);
                        }
                    }

                    Ast::Symbol(_, "sizeof") => {
                        if sexpr.len() == 2 {
                            SExpr::SizeOf {
                                meta: Metadata {
                                    range,
                                    type_: Type::UnknownInt,
                                },
                                type_: parse_type(sexpr.remove(1))?,
                            }
                        } else {
                            return Err(LoweringError::InvalidSizeOf);
                        }
                    }

                    // Function call
                    _ => SExpr::FuncCall {
                        meta: Metadata {
                            range,
                            type_: Type::Unknown,
                        },
                        func: Box::new(lower_helper(sexpr.remove(0), Annotations::default())?),
                        values: sexpr
                            .into_iter()
                            .map(|v| lower_helper(v, Annotations::default()))
                            .collect::<Result<Vec<SExpr>, LoweringError>>()?,
                    },
                }
            }
        }

        Ast::Attribute(range, value, attr) => match *attr {
            Ast::Symbol(_, "&") => SExpr::Ref {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                value: Box::new(lower_helper(*value, Annotations::default())?),
            },

            Ast::Symbol(_, "*") => SExpr::Deref {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                value: Box::new(lower_helper(*value, Annotations::default())?),
            },

            Ast::Symbol(_, attr) => SExpr::Attribute {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                top: Box::new(lower_helper(*value, Annotations::default())?),
                attr: attr.to_string(),
            },

            Ast::SExpr(_, mut v) if v.len() == 1 => SExpr::SliceGet {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                top: Box::new(lower_helper(*value, Annotations::default())?),
                index: Box::new(lower_helper(v.remove(0), Annotations::default())?),
            },

            v @ Ast::SExpr(_, _) => SExpr::SliceGet {
                meta: Metadata {
                    range,
                    type_: Type::Unknown,
                },
                top: Box::new(lower_helper(*value, Annotations::default())?),
                index: Box::new(lower_helper(v, Annotations::default())?),
            },

            _ => unreachable!("parser emits only symbols or s expressions"),
        },
    };

    Ok(sexpr)
}

pub fn lower(asts: Vec<Ast<'_>>) -> Result<Vec<SExpr>, LoweringError> {
    let mut sexprs = vec![];

    let mut ann = Annotations::default();
    for ast in asts {
        match ast {
            Ast::SExpr(_, v)
                if v.len() == 1 && matches!(v.get(0), Some(Ast::Symbol(_, "public"))) =>
            {
                ann.public = true;
            }

            _ => {
                sexprs.push(lower_helper(ast, ann)?);
                ann = Annotations::default();
            }
        }
    }

    Ok(sexprs)
}
