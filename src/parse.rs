use std::fmt::Display;

use crate::lexer::{Lexer, Span, Token};

#[derive(Debug, Clone)]
pub enum BaseType {
    Bottom,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Named(String, Vec<Type>, Vec<Ast>),
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Bottom => write!(f, "!"),
            BaseType::Bool => write!(f, "bool"),
            BaseType::I8 => write!(f, "i8"),
            BaseType::I16 => write!(f, "i16"),
            BaseType::I32 => write!(f, "i32"),
            BaseType::I64 => write!(f, "i64"),
            BaseType::U8 => write!(f, "u8"),
            BaseType::U16 => write!(f, "u16"),
            BaseType::U32 => write!(f, "u32"),
            BaseType::U64 => write!(f, "u64"),
            BaseType::F32 => write!(f, "f32"),
            BaseType::F64 => write!(f, "f64"),
            BaseType::Named(name, tparams, vparams) => {
                write!(f, "({}", name)?;
                for tparam in tparams {
                    write!(f, " {}", tparam)?;
                }
                for vparam in vparams {
                    write!(f, " {}", vparam)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Base(BaseType),
    Func(Box<Type>, Box<Type>),
    Refined(BaseType, Box<Ast>),
    Generic(String),
    TypeVar(usize),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "<unknown>"),
            Type::Base(b) => write!(f, "{}", b),
            Type::Func(a, r) => write!(f, "({} -> {})", a, r),
            Type::Refined(t, v) => write!(f, "{{ {} : {} }}", t, v),
            Type::Generic(g) => write!(f, "{}", g),
            Type::TypeVar(v) => write!(f, "${}", v),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    LogicalAnd,
    LogicalOr,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Ne => write!(f, "!="),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(Span),
    Symbol(Span, String),
    Constructor(Span, String, Vec<Pattern>),
    SymbolOrUnitConstructor(Span, String),
    As(Span, String, Box<Pattern>),
    Or(Span, Vec<Pattern>),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Wildcard(_) => write!(f, "_"),
            Pattern::Symbol(_, s) | Pattern::SymbolOrUnitConstructor(_, s) => write!(f, "{}", s),

            Pattern::Constructor(_, cons, pats) => {
                write!(f, "({}", cons)?;
                for pat in pats {
                    write!(f, " {}", pat)?;
                }
                write!(f, ")")
            }

            Pattern::As(_, name, pat) => write!(f, "({} @ {})", name, pat),

            Pattern::Or(_, pats) => {
                let mut first = true;
                for pat in pats {
                    if first {
                        write!(f, "({})", pat)?;
                        first = false;
                    } else {
                        write!(f, " | ({})", pat)?;
                    }
                }

                Ok(())
            }
        }
    }
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span)
            | Pattern::Symbol(span, _)
            | Pattern::Constructor(span, _, _)
            | Pattern::SymbolOrUnitConstructor(span, _)
            | Pattern::As(span, _, _)
            | Pattern::Or(span, _) => span.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Integer(Span, u128),
    Bool(Span, bool),
    Symbol(Span, String),

    Binary {
        span: Span,
        op: BinaryOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },

    FuncCall {
        span: Span,
        func: Box<Ast>,
        args: Vec<Ast>,
    },

    Let {
        span: Span,
        mutable: bool,
        symbol: String,
        args: Vec<(String, Type)>,
        ret_type: Type,
        value: Box<Ast>,
        context: Box<Ast>,
    },

    EmptyLet {
        span: Span,
        symbol: String,
        args: Vec<(String, Type)>,
        ret_type: Type,
    },

    TopLet {
        span: Span,
        symbol: String,
        constraints: Vec<(String, Vec<Type>)>,
        generics: Vec<String>,
        dep_values: Vec<(String, Type)>,
        args: Vec<(String, Type)>,
        ret_type: Type,
        value: Box<Ast>,
    },

    If {
        span: Span,
        cond: Box<Ast>,
        then: Box<Ast>,
        elsy: Box<Ast>,
    },

    DatatypeDefinition {
        span: Span,
        name: String,
        constraints: Vec<(String, Vec<Type>)>,
        generics: Vec<String>,
        dep_values: Vec<(String, Type)>,
        variants: Vec<(String, Vec<(Option<String>, Type)>)>,
    },

    Match {
        span: Span,
        value: Box<Ast>,
        patterns: Vec<(Pattern, Ast)>,
    },

    Class {
        span: Span,
        name: String,
        generics: Vec<String>,
        constraints: Vec<(String, Vec<Type>)>,
        functions: Vec<Ast>,
    },

    Instance {
        span: Span,
        name: String,
        parameters: Vec<Type>,
        generics: Vec<String>,
        constraints: Vec<(String, Vec<Type>)>,
        functions: Vec<Ast>,
    },
}

impl Ast {
    pub fn span(&self) -> Span {
        match self {
            Ast::Integer(span, _)
            | Ast::Bool(span, _)
            | Ast::Symbol(span, _)
            | Ast::Binary { span, .. }
            | Ast::FuncCall { span, .. }
            | Ast::Let { span, .. }
            | Ast::EmptyLet { span, .. }
            | Ast::TopLet { span, .. }
            | Ast::If { span, .. }
            | Ast::DatatypeDefinition { span, .. }
            | Ast::Match { span, .. }
            | Ast::Class { span, .. }
            | Ast::Instance { span, .. } => span.clone(),
        }
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Integer(_, n) => write!(f, "{}", n),
            Ast::Bool(_, b) => write!(f, "{}", b),
            Ast::Symbol(_, s) => write!(f, "{}", s),
            Ast::Binary {
                op, left, right, ..
            } => write!(f, "({} {} {})", left, op, right),

            Ast::FuncCall { func, args, .. } => {
                write!(f, "({}", func)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }

            Ast::Let {
                mutable,
                symbol,
                args,
                ret_type,
                value,
                context,
                ..
            } => {
                write!(f, "let ")?;
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", symbol)?;
                for (arg, type_) in args {
                    write!(f, " ({}: {})", arg, type_)?;
                }

                if args.is_empty() {
                    write!(f, ": {}", ret_type)?;
                } else {
                    write!(f, " -> {}", ret_type)?;
                }

                write!(f, " = {} in {}", value, context)
            }

            Ast::EmptyLet {
                symbol,
                args,
                ret_type,
                ..
            } => {
                write!(f, "let {}", symbol)?;
                for (arg, type_) in args {
                    write!(f, " ({}: {})", arg, type_)?;
                }

                if args.is_empty() {
                    write!(f, ": {}", ret_type)
                } else {
                    write!(f, " -> {}", ret_type)
                }
            }

            Ast::TopLet {
                symbol,
                generics,
                constraints,
                dep_values,
                args,
                ret_type,
                value,
                ..
            } => {
                if !generics.is_empty() || !dep_values.is_empty() {
                    write!(f, "forall")?;
                    for generic in generics {
                        write!(f, " ({}: type)", generic)?;
                    }
                    for (value, type_) in dep_values {
                        write!(f, " ({}: {})", value, type_)?;
                    }
                    if !constraints.is_empty() {
                        write!(f, " where ")?;
                        let mut first = true;
                        for (name, params) in constraints {
                            if first {
                                first = false;
                            } else {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", name)?;
                            for param in params {
                                write!(f, " {}", param)?;
                            }
                        }
                    }

                    writeln!(f)?;
                }

                write!(f, "let {}", symbol)?;
                for (arg, type_) in args {
                    write!(f, " ({}: {})", arg, type_)?;
                }

                if args.is_empty() {
                    write!(f, ": {}", ret_type)?;
                } else {
                    write!(f, " -> {}", ret_type)?;
                }

                write!(f, " = {}", value)
            }

            Ast::If {
                cond, then, elsy, ..
            } => write!(f, "(if {} then {} else {})", cond, then, elsy),

            Ast::DatatypeDefinition {
                name,
                generics,
                constraints,
                dep_values,
                variants,
                ..
            } => {
                if !generics.is_empty() || !dep_values.is_empty() {
                    write!(f, "forall")?;
                    for generic in generics {
                        write!(f, " ({}: type)", generic)?;
                    }
                    for (value, type_) in dep_values {
                        write!(f, " ({}: {})", value, type_)?;
                    }

                    if !constraints.is_empty() {
                        write!(f, " where ")?;
                        let mut first = true;
                        for (name, params) in constraints {
                            if first {
                                first = false;
                            } else {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", name)?;
                            for param in params {
                                write!(f, " {}", param)?;
                            }
                        }
                    }

                    writeln!(f)?;
                }

                writeln!(f, "type {}", name)?;
                let mut first = true;
                for (constructor, fields) in variants {
                    if first {
                        write!(f, "    = {}", constructor)?;
                        first = false;
                    } else {
                        write!(f, "    | {}", constructor)?;
                    }

                    for (name, type_) in fields {
                        if let Some(name) = name {
                            write!(f, " ({}: {})", name, type_)?;
                        } else {
                            write!(f, " {}", type_)?;
                        }
                    }
                    writeln!(f)?;
                }

                Ok(())
            }

            Ast::Match {
                value, patterns, ..
            } => {
                writeln!(f, "match {} with", value)?;
                for (pat, val) in patterns {
                    writeln!(f, "| {} to {}", pat, val)?;
                }
                writeln!(f, "end")
            }

            Ast::Class {
                name,
                generics,
                constraints,
                functions,
                ..
            } => {
                write!(f, "class {}", name)?;
                for generic in generics {
                    write!(f, " {}", generic)?;
                }
                if !constraints.is_empty() {
                    write!(f, " where ")?;
                    let mut first = true;
                    for (name, params) in constraints {
                        if first {
                            first = false;
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", name)?;
                        for param in params {
                            write!(f, " {}", param)?;
                        }
                    }
                }
                writeln!(f, " =")?;
                for func in functions {
                    writeln!(f, "{}", func)?;
                }

                writeln!(f, "end")
            }

            Ast::Instance {
                name,
                generics,
                constraints,
                parameters,
                functions,
                ..
            } => {
                if !generics.is_empty() {
                    write!(f, "forall")?;
                    for generic in generics {
                        write!(f, " ({}: type)", generic)?;
                    }

                    if !constraints.is_empty() {
                        write!(f, " where ")?;
                        let mut first = true;
                        for (name, params) in constraints {
                            if first {
                                first = false;
                            } else {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", name)?;
                            for param in params {
                                write!(f, " {}", param)?;
                            }
                        }
                    }

                    writeln!(f)?;
                }

                write!(f, "instance {}", name)?;
                for param in parameters {
                    write!(f, " {}", param)?;
                }
                writeln!(f, " =")?;
                for func in functions {
                    writeln!(f, "{}", func)?;
                }

                writeln!(f, "end")
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    NotStarted,
    Error {
        message: String,
        primary_label: String,
        primary_label_loc: Span,
        secondary_labels: Vec<(String, Span)>,
        notes: Vec<String>,
    },
}

macro_rules! try_token {
    ($lexer: expr, $pat: pat) => {{
        let token = $lexer.peek();
        if matches!(token, $pat) {
            $lexer.lex()
        } else {
            None
        }
    }};
}

fn parse_base_type(
    lexer: &mut Lexer<'_>,
    generics: &[String],
    parse_names: bool,
) -> Result<Type, ParseError> {
    let state = lexer.push();
    match lexer.lex() {
        Some((Token::Exclamation, _)) => Ok(Type::Base(BaseType::Bottom)),
        Some((Token::Symbol("bool"), _)) => Ok(Type::Base(BaseType::Bool)),
        Some((Token::Symbol("i8"), _)) => Ok(Type::Base(BaseType::I8)),
        Some((Token::Symbol("i16"), _)) => Ok(Type::Base(BaseType::I16)),
        Some((Token::Symbol("i32"), _)) => Ok(Type::Base(BaseType::I32)),
        Some((Token::Symbol("i64"), _)) => Ok(Type::Base(BaseType::I64)),
        Some((Token::Symbol("u8"), _)) => Ok(Type::Base(BaseType::U8)),
        Some((Token::Symbol("u16"), _)) => Ok(Type::Base(BaseType::U16)),
        Some((Token::Symbol("u32"), _)) => Ok(Type::Base(BaseType::U32)),
        Some((Token::Symbol("u64"), _)) => Ok(Type::Base(BaseType::U64)),
        Some((Token::Symbol("f32"), _)) => Ok(Type::Base(BaseType::F32)),
        Some((Token::Symbol("f64"), _)) => Ok(Type::Base(BaseType::F64)),
        Some((Token::Symbol(s), _)) if generics.iter().any(|v| v.as_str() == s) => {
            Ok(Type::Generic(s.to_string()))
        }
        Some((Token::Symbol(s), _)) if parse_names => Ok(Type::Base(BaseType::Named(
            s.to_string(),
            Vec::new(),
            Vec::new(),
        ))),

        Some((Token::LParen, s1)) => {
            let type_ = parse_type(lexer, generics)?;
            match lexer.lex() {
                Some((Token::RParen, _)) => Ok(type_),
                Some((_, s2)) => Err(ParseError::Error {
                    message: "expected right parenthesis".to_string(),
                    primary_label: "this is not a right parenthesis".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                    notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                }),
                None => Err(ParseError::Error {
                    message: "expected right parenthesis".to_string(),
                    primary_label: "eof found here".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                    notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                }),
            }
        }

        Some((Token::LBrace, _)) => {
            lexer.pop(state);
            parse_refinement_type(lexer, generics)
        }

        _ => {
            lexer.pop(state);
            Err(ParseError::NotStarted)
        }
    }
}

fn parse_child_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Type, ParseError> {
    match parse_base_type(lexer, generics, false) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match lexer.lex() {
        Some((Token::Symbol(s), _)) => Ok(Type::Base(BaseType::Named(
            s.to_string(),
            Vec::new(),
            Vec::new(),
        ))),

        Some((_, span)) => Err(ParseError::Error {
            message: "expected type".to_string(),
            primary_label: "this is not a valid type".to_string(),
            primary_label_loc: span,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),

        None => Err(ParseError::Error {
            message: "expected type".to_string(),
            primary_label: "eof found here".to_string(),
            primary_label_loc: lexer.loc()..lexer.loc() + 1,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),
    }
}

fn parse_parametarised_type(
    lexer: &mut Lexer<'_>,
    generics: &[String],
) -> Result<Type, ParseError> {
    match parse_base_type(lexer, generics, false) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match lexer.lex() {
        Some((Token::Symbol(s), _)) => {
            let mut parameters = Vec::new();
            let mut state = lexer.push();
            while let Ok(param) = parse_child_type(lexer, generics) {
                parameters.push(param);
                state = lexer.push();
            }
            lexer.pop(state);

            Ok(Type::Base(BaseType::Named(
                s.to_string(),
                parameters,
                Vec::new(),
            )))
        }

        Some((_, span)) => Err(ParseError::Error {
            message: "expected type".to_string(),
            primary_label: "this is not a valid type".to_string(),
            primary_label_loc: span,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),

        None => Err(ParseError::Error {
            message: "expected type".to_string(),
            primary_label: "eof found here".to_string(),
            primary_label_loc: lexer.loc()..lexer.loc() + 1,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),
    }
}

#[allow(unreachable_code)]
fn parse_refinement_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Type, ParseError> {
    if try_token!(lexer, Some((Token::LBrace, _))).is_some() {
        let base = match parse_parametarised_type(lexer, generics)? {
            Type::Base(v) => v,
            _ => {
                return Err(ParseError::Error {
                    message: todo!(),
                    primary_label: todo!(),
                    primary_label_loc: todo!(),
                    secondary_labels: todo!(),
                    notes: todo!(),
                })
            }
        };

        if try_token!(lexer, Some((Token::Colon, _))).is_none() {
            return Err(ParseError::Error {
                message: todo!(),
                primary_label: todo!(),
                primary_label_loc: todo!(),
                secondary_labels: todo!(),
                notes: todo!(),
            });
        }

        let constraint = parse_expr(lexer)?;

        if try_token!(lexer, Some((Token::RBrace, _))).is_none() {
            return Err(ParseError::Error {
                message: todo!(),
                primary_label: todo!(),
                primary_label_loc: todo!(),
                secondary_labels: todo!(),
                notes: todo!(),
            });
        }

        Ok(Type::Refined(base, Box::new(constraint)))
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_func_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Type, ParseError> {
    let mut top = parse_parametarised_type(lexer, generics)?;
    let mut nodes = Vec::new();
    while try_token!(lexer, Some((Token::Arrow, _))).is_some() {
        nodes.push(top);
        top = parse_parametarised_type(lexer, generics)?;
    }
    for node in nodes.into_iter().rev() {
        top = Type::Func(Box::new(node), Box::new(top));
    }

    Ok(top)
}

fn parse_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Type, ParseError> {
    parse_func_type(lexer, generics)
}

fn parse_value(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    match lexer.lex() {
        Some((Token::Integer(n), span)) => Ok(Ast::Integer(span, n)),
        Some((Token::Bool(b), span)) => Ok(Ast::Bool(span, b)),
        Some((Token::Symbol(s), span)) => Ok(Ast::Symbol(span, s.to_string())),
        Some((Token::LParen, s1)) => {
            let value = parse_expr(lexer)?;
            match lexer.lex() {
                Some((Token::RParen, _)) => Ok(value),
                Some((_, s2)) => Err(ParseError::Error {
                    message: "expected right parenthesis".to_string(),
                    primary_label: "this is not a right parenthesis".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                    notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                }),
                None => Err(ParseError::Error {
                    message: "expected right parenthesis".to_string(),
                    primary_label: "eof found here".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                    notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                }),
            }
        }

        Some((_, span)) => Err(ParseError::Error {
            message: "expected value".to_string(),
            primary_label: "this is not a value".to_string(),
            primary_label_loc: span,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),

        None => Err(ParseError::Error {
            message: "expected value".to_string(),
            primary_label: "eof found here".to_string(),
            primary_label_loc: lexer.loc()..lexer.loc() + 1,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),
    }
}

fn parse_func_call(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let func = parse_value(lexer)?;
    let mut args = Vec::new();
    let mut state = lexer.push();
    while let Ok(arg) = parse_value(lexer) {
        args.push(arg);
        state = lexer.push();
    }
    lexer.pop(state);

    if args.is_empty() {
        Ok(func)
    } else {
        Ok(Ast::FuncCall {
            span: func.span().start..args.last().unwrap().span().end,
            func: Box::new(func),
            args,
        })
    }
}

fn parse_muldiv(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_func_call(lexer)?;
    while let Some((token, _)) = try_token!(
        lexer,
        Some((Token::Astrisk | Token::Slash | Token::Percent, _))
    ) {
        let right = match parse_func_call(lexer) {
            Ok(v) => v,
            e @ Err(_) => {
                lexer.pop(state);
                return e;
            }
        };
        top = Ast::Binary {
            span: top.span().start..right.span().end,
            op: match token {
                Token::Astrisk => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            },
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_addsub(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_muldiv(lexer)?;
    while let Some((token, _)) = try_token!(lexer, Some((Token::Plus | Token::Minus, _))) {
        let right = match parse_muldiv(lexer) {
            Ok(v) => v,
            e @ Err(_) => {
                lexer.pop(state);
                return e;
            }
        };

        top = Ast::Binary {
            span: top.span().start..right.span().end,
            op: match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            },
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_compare(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_addsub(lexer)?;
    while let Some((token, _)) = try_token!(
        lexer,
        Some((
            Token::Lt | Token::Le | Token::Gt | Token::Ge | Token::Eq | Token::Ne,
            _
        ))
    ) {
        let right = match parse_addsub(lexer) {
            Ok(v) => v,
            e @ Err(_) => {
                lexer.pop(state);
                return e;
            }
        };

        top = Ast::Binary {
            span: top.span().start..right.span().end,
            op: match token {
                Token::Lt => BinaryOp::Lt,
                Token::Le => BinaryOp::Le,
                Token::Gt => BinaryOp::Gt,
                Token::Ge => BinaryOp::Ge,
                Token::Eq => BinaryOp::Eq,
                Token::Ne => BinaryOp::Ne,
                _ => unreachable!(),
            },
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_and(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_compare(lexer)?;
    while try_token!(lexer, Some((Token::LogicalAnd, _))).is_some() {
        let right = match parse_compare(lexer) {
            Ok(v) => v,
            e @ Err(_) => {
                lexer.pop(state);
                return e;
            }
        };

        top = Ast::Binary {
            span: top.span().start..right.span().end,
            op: BinaryOp::LogicalAnd,
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_or(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_and(lexer)?;
    while try_token!(lexer, Some((Token::LogicalOr, _))).is_some() {
        let right = match parse_and(lexer) {
            Ok(v) => v,
            e @ Err(_) => {
                lexer.pop(state);
                return e;
            }
        };

        top = Ast::Binary {
            span: top.span().start..right.span().end,
            op: BinaryOp::LogicalAnd,
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_argument(
    lexer: &mut Lexer<'_>,
    allow_type: bool,
    generics: &[String],
) -> Result<(String, Option<Type>), ParseError> {
    if let Some((Token::Symbol(s), _)) = try_token!(lexer, Some((Token::Symbol(_), _))) {
        Ok((s.to_string(), None))
    } else if let Some((_, s1)) = try_token!(lexer, Some((Token::LParen, _))) {
        if let Some((Token::Symbol(arg), _)) = try_token!(lexer, Some((Token::Symbol(_), _))) {
            match lexer.lex() {
                Some((Token::Colon, _)) => (),

                Some((_, s2)) => {
                    return Err(ParseError::Error {
                        message: "expected colon".to_string(),
                        primary_label: "this is not a colon".to_string(),
                        primary_label_loc: s2,
                        secondary_labels: vec![(
                            "argument declaration starts here".to_string(),
                            s1,
                        )],
                        notes: Vec::new(),
                    });
                }

                None => {
                    return Err(ParseError::Error {
                        message: "expected colon".to_string(),
                        primary_label: "eof found here".to_string(),
                        primary_label_loc: lexer.loc()..lexer.loc() + 1,
                        secondary_labels: vec![(
                            "argument declaration starts here".to_string(),
                            s1,
                        )],
                        notes: Vec::new(),
                    });
                }
            }

            let type_ = if allow_type && try_token!(lexer, Some((Token::Type, _))).is_some() {
                None
            } else {
                Some(parse_type(lexer, generics)?)
            };

            match lexer.lex() {
                Some((Token::RParen, _)) => (),
                Some((_, s2)) => {
                    return Err(ParseError::Error {
                        message: "expected right parenthesis".to_string(),
                        primary_label: "this is not a right parenthesis".to_string(),
                        primary_label_loc: s2,
                        secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                        notes: Vec::new(),
                    });
                }

                None => {
                    return Err(ParseError::Error {
                        message: "expected colon".to_string(),
                        primary_label: "eof found here".to_string(),
                        primary_label_loc: lexer.loc()..lexer.loc() + 1,
                        secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                        notes: Vec::new(),
                    });
                }
            }

            Ok((arg.to_string(), type_))
        } else {
            match lexer.lex() {
                Some((_, span)) => Err(ParseError::Error {
                    message: "expected argument name".to_string(),
                    primary_label: "this is not a valid argument name".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                }),

                None => Err(ParseError::Error {
                    message: "expected argument name".to_string(),
                    primary_label: "eof found here".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                }),
            }
        }
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_if(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some((_, s1 @ Span { start, .. })) = try_token!(lexer, Some((Token::If, _))) {
        let cond = parse_expr(lexer)?;
        match lexer.lex() {
            Some((Token::Then, _)) => (),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid if expression".to_string(),
                    primary_label: "expected `then`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("if expression starts here".to_string(), s1)],
                    notes: Vec::new(),
                });
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid if expression".to_string(),
                    primary_label: "expected `then`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("if expression starts here".to_string(), s1)],
                    notes: Vec::new(),
                });
            }
        }

        let then = parse_expr(lexer)?;
        match lexer.lex() {
            Some((Token::Else, _)) => (),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid if expression".to_string(),
                    primary_label: "expected `else`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("if expression starts here".to_string(), s1)],
                    notes: Vec::new(),
                });
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid if expression".to_string(),
                    primary_label: "expected `else`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("if expression starts here".to_string(), s1)],
                    notes: Vec::new(),
                });
            }
        }
        let elsy = parse_expr(lexer)?;

        Ok(Ast::If {
            span: start..elsy.span().end,
            cond: Box::new(cond),
            then: Box::new(then),
            elsy: Box::new(elsy),
        })
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_pattern_child(lexer: &mut Lexer<'_>) -> Result<Pattern, ParseError> {
    match lexer.peek() {
        Some((Token::Symbol("_"), span)) => {
            lexer.lex();
            Ok(Pattern::Wildcard(span))
        }

        Some((Token::Symbol(name), span)) => {
            lexer.lex();

            match lexer.peek() {
                Some((Token::At, s2)) => {
                    lexer.lex();
                    match parse_pattern_child(lexer)? {
                        Pattern::As(s3, _, _) => Err(ParseError::Error {
                            message: "cannot nest `@` pattern operator".to_string(),
                            primary_label: "second instance of `@` pattern operator".to_string(),
                            primary_label_loc: s3,
                            secondary_labels: vec![(
                                "first instance of `@` pattern operator".to_string(),
                                s2,
                            )],
                            notes: Vec::new(),
                        }),
                        v => Ok(Pattern::As(
                            span.start..v.span().end,
                            name.to_string(),
                            Box::new(v),
                        )),
                    }
                }

                Some(_) => {
                    let mut fields = Vec::new();
                    loop {
                        match lexer.peek() {
                            Some((Token::Symbol("_"), span)) => {
                                fields.push(Pattern::Wildcard(span))
                            }
                            Some((Token::Symbol(v), span)) => {
                                fields.push(Pattern::SymbolOrUnitConstructor(span, v.to_string()))
                            }
                            Some((Token::LParen, s1)) => {
                                lexer.lex();
                                fields.push(parse_pattern(lexer)?);
                                match lexer.peek() {
                                    Some((Token::RParen, _)) => (),
                                    Some((_, s2)) => {
                                        return Err(ParseError::Error {
                                            message: "expected right parenthesis".to_string(),
                                            primary_label: "this is not a right parenthesis"
                                                .to_string(),
                                            primary_label_loc: s2,
                                            secondary_labels: vec![(
                                                "left parenthesis found here".to_string(),
                                                s1,
                                            )],
                                            notes: vec![
                                                "insert a right parenthesis to resolve this error"
                                                    .to_string(),
                                            ],
                                        });
                                    }
                                    None => {
                                        return Err(ParseError::Error {
                                            message: "expected right parenthesis".to_string(),
                                            primary_label: "eof found here".to_string(),
                                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                                            secondary_labels: vec![(
                                                "left parenthesis found here".to_string(),
                                                s1,
                                            )],
                                            notes: vec![
                                                "insert a right parenthesis to resolve this error"
                                                    .to_string(),
                                            ],
                                        });
                                    }
                                }
                            }

                            _ => break,
                        }

                        lexer.lex();
                    }

                    if fields.is_empty() {
                        Ok(Pattern::SymbolOrUnitConstructor(span, name.to_string()))
                    } else {
                        Ok(Pattern::Constructor(
                            span.start..fields.last().unwrap().span().end,
                            name.to_string(),
                            fields,
                        ))
                    }
                }

                _ => Ok(Pattern::SymbolOrUnitConstructor(span, name.to_string())),
            }
        }

        Some((Token::LParen, s1)) => {
            lexer.lex();
            let v = parse_pattern(lexer)?;
            match lexer.lex() {
                Some((Token::RParen, _)) => (),

                Some((_, s2)) => {
                    return Err(ParseError::Error {
                        message: "expected right parenthesis".to_string(),
                        primary_label: "this is not a right parenthesis".to_string(),
                        primary_label_loc: s2,
                        secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                        notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                    });
                }

                None => {
                    return Err(ParseError::Error {
                        message: "expected right parenthesis".to_string(),
                        primary_label: "eof found here".to_string(),
                        primary_label_loc: lexer.loc()..lexer.loc() + 1,
                        secondary_labels: vec![("left parenthesis found here".to_string(), s1)],
                        notes: vec!["insert a right parenthesis to resolve this error".to_string()],
                    });
                }
            }

            Ok(v)
        }

        Some((_, span)) => Err(ParseError::Error {
            message: "invalid pattern".to_string(),
            primary_label: "expected a pattern".to_string(),
            primary_label_loc: span,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),

        None => Err(ParseError::Error {
            message: "expected a pattern".to_string(),
            primary_label: "eof found here".to_string(),
            primary_label_loc: lexer.loc()..lexer.loc() + 1,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }),
    }
}

fn parse_pattern(lexer: &mut Lexer<'_>) -> Result<Pattern, ParseError> {
    let mut children = vec![parse_pattern_child(lexer)?];

    while try_token!(lexer, Some((Token::Pipe, _))).is_some() {
        children.push(parse_pattern_child(lexer)?);
    }

    if children.len() == 1 {
        Ok(children.remove(0))
    } else {
        Ok(Pattern::Or(
            children.first().unwrap().span().start..children.last().unwrap().span().end,
            children,
        ))
    }
}

fn parse_match(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some((_, span @ Span { start, .. })) = try_token!(lexer, Some((Token::Match, _))) {
        let value = parse_expr(lexer)?;
        match lexer.lex() {
            Some((Token::With, _)) => (),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid match expression".to_string(),
                    primary_label: "expected `with`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("match expression starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid match expression".to_string(),
                    primary_label: "expected `with`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("match expression starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
        }

        let mut patterns = Vec::new();
        while try_token!(lexer, Some((Token::Pipe, _))).is_some() {
            let pattern = parse_pattern(lexer)?;

            match lexer.lex() {
                Some((Token::To, _)) => (),

                Some((_, s2)) => {
                    return Err(ParseError::Error {
                        message: "invalid match expression".to_string(),
                        primary_label: "expected `to`".to_string(),
                        primary_label_loc: s2,
                        secondary_labels: vec![("match expression starts here".to_string(), span)],
                        notes: Vec::new(),
                    });
                }

                None => {
                    return Err(ParseError::Error {
                        message: "invalid match expression".to_string(),
                        primary_label: "expected `to`".to_string(),
                        primary_label_loc: lexer.loc()..lexer.loc() + 1,
                        secondary_labels: vec![("match expression starts here".to_string(), span)],
                        notes: Vec::new(),
                    });
                }
            }

            let value = parse_expr(lexer)?;
            patterns.push((pattern, value));
        }

        match lexer.lex() {
            Some((Token::End, Span { end, .. })) => Ok(Ast::Match {
                span: start..end,
                value: Box::new(value),
                patterns,
            }),

            Some((_, s2)) => Err(ParseError::Error {
                message: "invalid match expression".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: s2,
                secondary_labels: vec![("match expression starts here".to_string(), span)],
                notes: Vec::new(),
            }),

            None => Err(ParseError::Error {
                message: "invalid match expression".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: lexer.loc()..lexer.loc() + 1,
                secondary_labels: vec![("match expression starts here".to_string(), span)],
                notes: Vec::new(),
            }),
        }
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_let(
    lexer: &mut Lexer<'_>,
    top_level: bool,
    generics: &[String],
    allow_no_body: bool,
    constraints: &[(String, Vec<Type>)],
) -> Result<Ast, ParseError> {
    if let Some((Token::Let, span @ Span { start, .. })) = lexer.peek() {
        lexer.lex();

        if !top_level && !allow_no_body && try_token!(lexer, Some((Token::LParen, _))).is_some() {
            let pattern = parse_pattern(lexer)?;

            if try_token!(lexer, Some((Token::RParen, _))).is_none()
                || try_token!(lexer, Some((Token::Equals, _))).is_none()
            {
                match lexer.lex() {
                    Some((_, s2)) => {
                        return Err(ParseError::Error {
                            message: "invalid let binding".to_string(),
                            primary_label: "expected `)` followed by `=`".to_string(),
                            primary_label_loc: s2,
                            secondary_labels: vec![("let binding starts here".to_string(), span)],
                            notes: Vec::new(),
                        });
                    }

                    None => {
                        return Err(ParseError::Error {
                            message: "invalid let binding".to_string(),
                            primary_label: "expected `)` followed by `=`".to_string(),
                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                            secondary_labels: vec![("let binding starts here".to_string(), span)],
                            notes: Vec::new(),
                        });
                    }
                }
            }

            let value = parse_expr(lexer)?;
            if try_token!(lexer, Some((Token::In, _))).is_none() {
                match lexer.lex() {
                    Some((_, s2)) => {
                        return Err(ParseError::Error {
                            message: "invalid let binding".to_string(),
                            primary_label: "expected `in`".to_string(),
                            primary_label_loc: s2,
                            secondary_labels: vec![("let binding starts here".to_string(), span)],
                            notes: Vec::new(),
                        });
                    }

                    None => {
                        return Err(ParseError::Error {
                            message: "invalid let binding".to_string(),
                            primary_label: "expected `in`".to_string(),
                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                            secondary_labels: vec![("let binding starts here".to_string(), span)],
                            notes: Vec::new(),
                        });
                    }
                }
            }
            let context = parse_expr(lexer)?;

            let mut patterns = vec![(pattern, context)];
            if let Some((_, span)) = try_token!(lexer, Some((Token::Else, _))) {
                patterns.push((Pattern::Wildcard(span), parse_expr(lexer)?));
            }

            return Ok(Ast::Match {
                span: start..patterns.last().unwrap().1.span().end,
                value: Box::new(value),
                patterns,
            });
        }

        let mutable = try_token!(lexer, Some((Token::Mut, _))).is_some();
        let symbol = match lexer.lex() {
            Some((Token::Symbol(v), _)) => v.to_string(),
            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid let binding".to_string(),
                    primary_label: "expected variable name".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("let binding starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }
            None => {
                return Err(ParseError::Error {
                    message: "invalid let binding".to_string(),
                    primary_label: "expected variable name".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("let binding starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }
        };

        let mut args = Vec::new();

        loop {
            match parse_argument(lexer, false, generics) {
                Ok((arg, Some(t))) => args.push((arg, t)),
                Ok((arg, None)) => args.push((arg, Type::Unknown)),
                Err(ParseError::NotStarted) => break,
                Err(e) => return Err(e),
            }
        }

        let ret_type = if if args.is_empty() {
            try_token!(lexer, Some((Token::Colon, _)))
        } else {
            try_token!(lexer, Some((Token::Arrow, _)))
        }
        .is_some()
        {
            parse_type(lexer, generics)?
        } else {
            Type::Unknown
        };

        if try_token!(lexer, Some((Token::Equals, _))).is_none() {
            let span = start..lexer.loc();
            if allow_no_body
                && !mutable
                && args.iter().all(|(_, t)| !matches!(t, Type::Unknown))
                && !matches!(ret_type, Type::Unknown)
            {
                return Ok(Ast::EmptyLet {
                    span,
                    symbol,
                    args,
                    ret_type,
                });
            } else if !allow_no_body {
                return Err(ParseError::Error {
                    message: "declaration is not allowed in this position".to_string(),
                    primary_label: "declaration is invalid here".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            } else if mutable {
                return Err(ParseError::Error {
                    message: "declaration cannot be mutable".to_string(),
                    primary_label: "this declaration is mutable".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            } else if args.iter().any(|(_, t)| matches!(t, Type::Unknown))
                || matches!(ret_type, Type::Unknown)
            {
                return Err(ParseError::Error {
                    message: "declaration must have types declared".to_string(),
                    primary_label: "this declaration has missing type declarations".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            }
        } else if allow_no_body {
            let (_, s2) = try_token!(lexer, Some((Token::Equals, _))).unwrap();
            return Err(ParseError::Error {
                message: "declaration must not have a body".to_string(),
                primary_label: "body starting here".to_string(),
                primary_label_loc: s2,
                secondary_labels: Vec::new(),
                notes: Vec::new(),
            });
        }

        let value = parse_expr(lexer)?;
        if try_token!(lexer, Some((Token::In, _))).is_none() {
            if top_level && !mutable {
                return Ok(Ast::TopLet {
                    span: start..value.span().end,
                    symbol,
                    generics: generics.to_vec(),
                    constraints: constraints.to_vec(),
                    dep_values: Vec::new(), // TODO
                    args,
                    ret_type,
                    value: Box::new(value),
                });
            } else if !top_level {
                match lexer.lex() {
                    Some((_, s2)) => {
                        return Err(ParseError::Error {
                            message: "invalid local definition".to_string(),
                            primary_label: "context expected here".to_string(),
                            primary_label_loc: s2,
                            secondary_labels: vec![(
                                "local definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        });
                    }

                    None => {
                        return Err(ParseError::Error {
                            message: "invalid local definition".to_string(),
                            primary_label: "context expected here".to_string(),
                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                            secondary_labels: vec![(
                                "local definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        });
                    }
                }
            } else if mutable {
                return Err(ParseError::Error {
                    message: "invalid top level definition".to_string(),
                    primary_label: "top level definition cannot be mutable".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            }
        } else if top_level {
            let (_, s2) = try_token!(lexer, Some((Token::In, _))).unwrap();
            return Err(ParseError::Error {
                message: "top level definition must not have a context".to_string(),
                primary_label: "context starting here".to_string(),
                primary_label_loc: s2,
                secondary_labels: vec![("top level definition starts here".to_string(), span)],
                notes: Vec::new(),
            });
        }

        let context = parse_expr(lexer)?;
        Ok(Ast::Let {
            span: start..0,
            mutable: false,
            symbol,
            args,
            ret_type,
            value: Box::new(value),
            context: Box::new(context),
        })
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_type_def_variant(
    lexer: &mut Lexer<'_>,
    generics: &[String],
) -> Result<(String, Vec<(Option<String>, Type)>), ParseError> {
    let name = match lexer.lex() {
        Some((Token::Symbol(v), _)) => v.to_string(),
        Some((_, span)) => {
            return Err(ParseError::Error {
                message: "invalid datatype variant".to_string(),
                primary_label: "expected symbol".to_string(),
                primary_label_loc: span,
                secondary_labels: Vec::new(),
                notes: Vec::new(),
            })
        }
        None => {
            return Err(ParseError::Error {
                message: "invalid datatype variant".to_string(),
                primary_label: "expected symbol".to_string(),
                primary_label_loc: lexer.loc()..lexer.loc() + 1,
                secondary_labels: Vec::new(),
                notes: Vec::new(),
            })
        }
    };

    let mut fields = Vec::new();
    loop {
        let t = match parse_base_type(lexer, generics, true) {
            Ok(v) => v,
            Err(ParseError::NotStarted) => break,
            Err(e) => return Err(e),
        };

        fields.push((None, t));
    }

    Ok((name, fields))
}

fn parse_type_def(
    lexer: &mut Lexer<'_>,
    generics: &[String],
    constraints: &[(String, Vec<Type>)],
) -> Result<Ast, ParseError> {
    if let Some((_, span @ Span { start, .. })) = try_token!(lexer, Some((Token::Type, _))) {
        let name = match lexer.lex() {
            Some((Token::Symbol(v), _)) => v.to_string(),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid type definition".to_string(),
                    primary_label: "expected datatype name".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("type definition starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid type definition".to_string(),
                    primary_label: "expected datatype name".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("type definition starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }
        };

        match lexer.lex() {
            Some((Token::Equals, _)) => (),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid type definition".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("type definition starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid type definition".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("type definition starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
        }

        let mut variants = vec![parse_type_def_variant(lexer, generics)?];
        while try_token!(lexer, Some((Token::Pipe, _))).is_some() {
            variants.push(parse_type_def_variant(lexer, generics)?);
        }

        Ok(Ast::DatatypeDefinition {
            span: start..lexer.loc(),
            name,
            generics: generics.to_vec(),
            constraints: constraints.to_vec(),
            dep_values: Vec::new(),
            variants,
        })
    } else {
        Err(ParseError::NotStarted)
    }
}

// TODO: dependent values
fn parse_forall(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some((_, span)) = try_token!(lexer, Some((Token::Forall, _))) {
        let mut generics = Vec::new();
        loop {
            match parse_argument(lexer, true, &[]) {
                Ok((generic, None)) => generics.push(generic.to_string()),
                Ok(_) => todo!(),
                Err(ParseError::NotStarted) => break,
                Err(e) => return Err(e),
            }
        }

        let mut constraints = Vec::new();
        if try_token!(lexer, Some((Token::Where, _))).is_some() {
            while {
                let name = match lexer.lex() {
                    Some((Token::Symbol(v), _)) => v.to_string(),

                    Some((_, s2)) => {
                        return Err(ParseError::Error {
                            message: "invalid class definition".to_string(),
                            primary_label: "expected constraint".to_string(),
                            primary_label_loc: s2,
                            secondary_labels: vec![(
                                "class definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        })
                    }

                    None => {
                        return Err(ParseError::Error {
                            message: "invalid class definition".to_string(),
                            primary_label: "expected constraint".to_string(),
                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                            secondary_labels: vec![(
                                "class definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        })
                    }
                };

                let mut parameters = Vec::new();
                loop {
                    match parse_base_type(lexer, &generics, true) {
                        Ok(v) => parameters.push(v),
                        Err(ParseError::NotStarted) => break,
                        Err(e) => return Err(e),
                    }
                }

                constraints.push((name, parameters));
                try_token!(lexer, Some((Token::Comma, _))).is_some()
            } {}
        }

        match parse_let(lexer, true, &generics, false, &constraints) {
            v @ Ok(_) => return v,
            Err(ParseError::NotStarted) => (),
            e => return e,
        }

        match parse_instance(lexer, &generics, &constraints) {
            v @ Ok(_) => return v,
            Err(ParseError::NotStarted) => (),
            e => return e,
        }

        parse_type_def(lexer, &generics, &constraints)
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_class(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some((_, span @ Span { start, .. })) = try_token!(lexer, Some((Token::Class, _))) {
        let name = match lexer.lex() {
            Some((Token::Symbol(v), _)) => v.to_string(),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid class definition".to_string(),
                    primary_label: "expected class name".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("class definition starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }

            _ => {
                return Err(ParseError::Error {
                    message: "invalid class definition".to_string(),
                    primary_label: "expected class name".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("class definition starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }
        };

        let mut generics = Vec::new();
        while let Some((Token::Symbol(generic), _)) = try_token!(lexer, Some((Token::Symbol(_), _)))
        {
            generics.push(generic.to_string());
        }

        let mut constraints = Vec::new();
        if try_token!(lexer, Some((Token::Where, _))).is_some() {
            while {
                let name = match lexer.lex() {
                    Some((Token::Symbol(v), _)) => v.to_string(),

                    Some((_, s2)) => {
                        return Err(ParseError::Error {
                            message: "invalid class definition".to_string(),
                            primary_label: "expected constraint".to_string(),
                            primary_label_loc: s2,
                            secondary_labels: vec![(
                                "class definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        })
                    }

                    None => {
                        return Err(ParseError::Error {
                            message: "invalid class definition".to_string(),
                            primary_label: "expected constraint".to_string(),
                            primary_label_loc: lexer.loc()..lexer.loc() + 1,
                            secondary_labels: vec![(
                                "class definition starts here".to_string(),
                                span,
                            )],
                            notes: Vec::new(),
                        })
                    }
                };

                let mut parameters = Vec::new();
                loop {
                    match parse_base_type(lexer, &generics, true) {
                        Ok(v) => parameters.push(v),
                        Err(ParseError::NotStarted) => break,
                        Err(e) => return Err(e),
                    }
                }

                constraints.push((name, parameters));
                try_token!(lexer, Some((Token::Comma, _))).is_some()
            } {}
        }

        match lexer.lex() {
            Some((Token::Equals, _)) => (),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid class definition".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("class definition starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid class definition".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("class definition starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
        }

        let mut functions = Vec::new();
        loop {
            match parse_let(lexer, true, &generics, true, &[]) {
                Ok(mut v) => {
                    if let Ast::TopLet { generics, .. } = &mut v {
                        *generics = Vec::new();
                    }
                    functions.push(v)
                }

                Err(ParseError::NotStarted) => break,
                e @ Err(_) => return e,
            }
        }

        match lexer.lex() {
            Some((Token::End, Span { end, .. })) => Ok(Ast::Class {
                span: start..end,
                name,
                generics,
                constraints,
                functions,
            }),

            Some((_, s2)) => Err(ParseError::Error {
                message: "invalid class definition".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: s2,
                secondary_labels: vec![("class definition starts here".to_string(), span)],
                notes: Vec::new(),
            }),

            None => Err(ParseError::Error {
                message: "invalid class definition".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: lexer.loc()..lexer.loc() + 1,
                secondary_labels: vec![("class definition starts here".to_string(), span)],
                notes: Vec::new(),
            }),
        }
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_instance(
    lexer: &mut Lexer<'_>,
    generics: &[String],
    constraints: &[(String, Vec<Type>)],
) -> Result<Ast, ParseError> {
    if let Some((_, span @ Span { start, .. })) = try_token!(lexer, Some((Token::Instance, _))) {
        let name = match lexer.lex() {
            Some((Token::Symbol(v), _)) => v.to_string(),

            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid instance".to_string(),
                    primary_label: "expected constraint".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("instance starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }

            None => {
                return Err(ParseError::Error {
                    message: "invalid instance".to_string(),
                    primary_label: "expected constraint".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("instance starts here".to_string(), span)],
                    notes: Vec::new(),
                })
            }
        };

        let mut parameters = Vec::new();
        loop {
            match parse_base_type(lexer, generics, true) {
                Ok(v) => parameters.push(v),
                Err(ParseError::NotStarted) => break,
                Err(e) => return Err(e),
            }
        }

        match lexer.lex() {
            Some((Token::Equals, _)) => (),
            Some((_, s2)) => {
                return Err(ParseError::Error {
                    message: "invalid instance".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("instance starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
            None => {
                return Err(ParseError::Error {
                    message: "invalid instance".to_string(),
                    primary_label: "expected `=`".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc() + 1,
                    secondary_labels: vec![("instance starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
        }

        let mut functions = Vec::new();
        loop {
            match parse_let(lexer, true, &generics, false, &constraints) {
                Ok(mut v) => {
                    if let Ast::TopLet {
                        generics,
                        constraints,
                        ..
                    } = &mut v
                    {
                        *generics = Vec::new();
                        *constraints = Vec::new();
                    }
                    functions.push(v);
                }

                Err(ParseError::NotStarted) => break,
                e @ Err(_) => return e,
            }
        }

        match lexer.lex() {
            Some((Token::End, Span { end, .. })) => Ok(Ast::Instance {
                span: start..end,
                name,
                generics: generics.to_vec(),
                constraints: constraints.to_vec(),
                parameters,
                functions,
            }),

            Some((_, s2)) => Err(ParseError::Error {
                message: "invalid instance".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: s2,
                secondary_labels: vec![("instance starts here".to_string(), span)],
                notes: Vec::new(),
            }),

            None => Err(ParseError::Error {
                message: "invalid instance".to_string(),
                primary_label: "expected `end`".to_string(),
                primary_label_loc: lexer.loc()..lexer.loc() + 1,
                secondary_labels: vec![("instance starts here".to_string(), span)],
                notes: Vec::new(),
            }),
        }
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_expr(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    match parse_let(lexer, false, &[], false, &[]) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e => return e,
    }
    match parse_if(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e => return e,
    }
    match parse_match(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e => return e,
    }
    parse_or(lexer)
}

fn parse_top(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    match parse_forall(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match parse_type_def(lexer, &[], &[]) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match parse_class(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match parse_instance(lexer, &[], &[]) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match parse_let(lexer, true, &[], false, &[]) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    if let Some((_, span)) = lexer.lex() {
        Err(ParseError::Error {
            message: "unexpected token".to_string(),
            primary_label: "expected `forall`, `type`, `class`, `instance`, or `let`".to_string(),
            primary_label_loc: span,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        })
    } else {
        unreachable!();
    }
}

pub fn parse(lexer: &mut Lexer<'_>) -> Result<Vec<Ast>, ParseError> {
    let mut asts = Vec::new();
    while lexer.peek().is_some() {
        asts.push(parse_top(lexer)?);
    }
    Ok(asts)
}
