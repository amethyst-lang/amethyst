use std::fmt::{Display, Formatter};

use crate::lexer::{Lexer, Span, Token};

#[derive(Debug, Clone, PartialEq)]
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
    Named(String, Vec<Monotype>),
}

impl Display for BaseType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
            BaseType::Named(name, tparams) => {
                write!(f, "({}", name)?;
                for tparam in tparams {
                    write!(f, " {}", tparam)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Monotype {
    Unknown,
    Base(BaseType),
    Func(Box<Monotype>, Box<Monotype>),
    Generic(String),
    TypeVar(usize),
}

impl Display for Monotype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Monotype::Unknown => write!(f, "<unknown>"),
            Monotype::Base(b) => write!(f, "{}", b),
            Monotype::Func(a, r) => write!(f, "({} -> {})", a, r),
            Monotype::Generic(g) => write!(f, "{}", g),
            Monotype::TypeVar(v) => write!(f, "${}", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub foralls: Vec<String>,
    pub constraints: Vec<(String, Vec<Type>)>,
    pub monotype: Monotype,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.foralls.is_empty() {
            write!(f, "forall")?;
            for t in self.foralls.iter() {
                write!(f, " {}", t)?;
            }
            write!(f, ". ")?;
        }

        for (cons, ts) in self.constraints.iter() {
            write!(f, "{}", cons)?;
            for t in ts {
                write!(f, " {}", t)?;
            }
            write!(f, " => ")?;
        }

        write!(f, "{}", self.monotype)
    }
}

impl Type {
    fn unknown() -> Self {
        Type {
            foralls: Vec::new(),
            constraints: Vec::new(),
            monotype: Monotype::Unknown,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
        op: String,
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
        args: Vec<String>,
        value: Box<Ast>,
        context: Box<Ast>,
    },

    EmptyLet {
        span: Span,
        symbol: String,
        type_: Type,
        args: Vec<String>,
    },

    TopLet {
        span: Span,
        symbol: String,
        args: Vec<String>,
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
        constraints: Vec<(String, Vec<Monotype>)>,
        generics: Vec<String>,
        variants: Vec<(String, Vec<(Option<String>, Monotype)>)>,
    },

    Match {
        span: Span,
        value: Box<Ast>,
        patterns: Vec<(Pattern, usize, Ast)>,
    },

    Class {
        span: Span,
        name: String,
        generics: Vec<String>,
        constraints: Vec<(String, Vec<Monotype>)>,
        functions: Vec<Ast>,
    },

    Instance {
        span: Span,
        name: String,
        generics: Vec<String>,
        parameters: Vec<Monotype>,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
                value,
                context,
                ..
            } => {
                write!(f, "let ")?;
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", symbol)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }

                write!(f, " = {} in {}", value, context)
            }

            Ast::EmptyLet {
                symbol,
                type_,
                args,
                ..
            } => {
                write!(f, "let {}", symbol)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }

                write!(f, " : {}", type_)
            }

            Ast::TopLet {
                symbol,
                args,
                value,
                ..
            } => {
                write!(f, "let {}", symbol)?;
                for arg in args {
                    write!(f, " {}", arg)?;
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
                variants,
                ..
            } => {
                write!(f, "type {}", name)?;
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

                writeln!(f)?;
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
                for (pat, _, val) in patterns {
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
) -> Result<Monotype, ParseError> {
    let state = lexer.push();
    match lexer.lex() {
        Some((Token::Exclamation, _)) => Ok(Monotype::Base(BaseType::Bottom)),
        Some((Token::Symbol("bool"), _)) => Ok(Monotype::Base(BaseType::Bool)),
        Some((Token::Symbol("i8"), _)) => Ok(Monotype::Base(BaseType::I8)),
        Some((Token::Symbol("i16"), _)) => Ok(Monotype::Base(BaseType::I16)),
        Some((Token::Symbol("i32"), _)) => Ok(Monotype::Base(BaseType::I32)),
        Some((Token::Symbol("i64"), _)) => Ok(Monotype::Base(BaseType::I64)),
        Some((Token::Symbol("u8"), _)) => Ok(Monotype::Base(BaseType::U8)),
        Some((Token::Symbol("u16"), _)) => Ok(Monotype::Base(BaseType::U16)),
        Some((Token::Symbol("u32"), _)) => Ok(Monotype::Base(BaseType::U32)),
        Some((Token::Symbol("u64"), _)) => Ok(Monotype::Base(BaseType::U64)),
        Some((Token::Symbol("f32"), _)) => Ok(Monotype::Base(BaseType::F32)),
        Some((Token::Symbol("f64"), _)) => Ok(Monotype::Base(BaseType::F64)),
        Some((Token::Symbol(s), _)) if generics.iter().any(|v| v.as_str() == s) => {
            Ok(Monotype::Generic(s.to_string()))
        }
        Some((Token::Symbol(s), _)) if parse_names => Ok(Monotype::Base(BaseType::Named(
            s.to_string(),
            Vec::new(),
        ))),

        Some((Token::LParen, s1)) => {
            let type_ = parse_func_type(lexer, generics)?;
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

        _ => {
            lexer.pop(state);
            Err(ParseError::NotStarted)
        }
    }
}

fn parse_child_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Monotype, ParseError> {
    match parse_base_type(lexer, generics, false) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    match lexer.lex() {
        Some((Token::Symbol(s), _)) => Ok(Monotype::Base(BaseType::Named(
            s.to_string(),
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
) -> Result<Monotype, ParseError> {
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

            Ok(Monotype::Base(BaseType::Named(
                s.to_string(),
                parameters,
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

fn parse_func_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Monotype, ParseError> {
    let mut top = parse_parametarised_type(lexer, generics)?;
    let mut nodes = Vec::new();
    while try_token!(lexer, Some((Token::Arrow, _))).is_some() {
        nodes.push(top);
        top = parse_parametarised_type(lexer, generics)?;
    }
    for node in nodes.into_iter().rev() {
        top = Monotype::Func(Box::new(node), Box::new(top));
    }

    Ok(top)
}

fn parse_type(lexer: &mut Lexer<'_>, generics: &[String]) -> Result<Type, ParseError> {
    let mut foralls = Vec::new();
    if try_token!(lexer, Some((Token::Forall, _))).is_some() {
        loop {
            match lexer.lex() {
                Some((Token::Symbol(s), _)) => foralls.push(s.to_string()),
                Some((Token::Dot, _)) => break,
                Some((_, span)) => return Err(ParseError::Error {
                    message: "invalid universal type".to_string(),
                    primary_label: "expected either a symbol or a period".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                }),
                None => return Err(ParseError::Error {
                    message: "invalid universal type".to_string(),
                    primary_label: "expected either a symbol or a period".to_string(),
                    primary_label_loc: lexer.loc()..lexer.loc(),
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                }),
            }
        }
    }

    let constraints = Vec::new(); // TODO

    let monotype = parse_func_type(lexer, generics)?;
    Ok(Type {
        foralls,
        constraints,
        monotype,
    })
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
                Token::Astrisk => "*",
                Token::Slash => "/",
                Token::Percent => "%",
                _ => unreachable!(),
            }.to_string(),
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
                Token::Plus => "+",
                Token::Minus => "-",
                _ => unreachable!(),
            }.to_string(),
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
                Token::Lt => "<",
                Token::Le => "<=",
                Token::Gt => ">",
                Token::Ge => ">=",
                Token::Eq => "==",
                Token::Ne => "!=",
                _ => unreachable!(),
            }.to_string(),
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
            op: "&&".to_string(),
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
            op: "||".to_string(),
            left: Box::new(top),
            right: Box::new(right),
        };
    }

    Ok(top)
}

fn parse_argument(
    lexer: &mut Lexer<'_>,
) -> Result<String, ParseError> {
    if let Some((Token::Symbol(s), _)) = try_token!(lexer, Some((Token::Symbol(_), _))) {
        Ok(s.to_string())
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
            patterns.push((pattern, 0, value));
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

            let mut patterns = vec![(pattern, 0, context)];
            if let Some((_, span)) = try_token!(lexer, Some((Token::Else, _))) {
                patterns.push((Pattern::Wildcard(span), 0, parse_expr(lexer)?));
            }

            return Ok(Ast::Match {
                span: start..patterns.last().unwrap().2.span().end,
                value: Box::new(value),
                patterns,
            });
        }

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
            match parse_argument(lexer) {
                Ok(arg) => args.push(arg),
                Err(ParseError::NotStarted) => break,
                Err(e) => return Err(e),
            }
        }

        let type_ = if allow_no_body && try_token!(lexer, Some((Token::Colon, _))).is_some() {
            parse_type(lexer, generics)?
        } else {
            Type::unknown()
        };

        if try_token!(lexer, Some((Token::Equals, _))).is_none() {
            let span = start..lexer.loc();
            if allow_no_body && !matches!(type_.monotype, Monotype::Unknown) {
                return Ok(Ast::EmptyLet {
                    span,
                    symbol,
                    type_,
                    args,
                });
            } else if !allow_no_body {
                return Err(ParseError::Error {
                    message: "declaration is not allowed in this position".to_string(),
                    primary_label: "declaration is invalid here".to_string(),
                    primary_label_loc: span,
                    secondary_labels: Vec::new(),
                    notes: Vec::new(),
                });
            } else if matches!(type_.monotype, Monotype::Unknown) {
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
        if let Some((_, s2)) = try_token!(lexer, Some((Token::In, _))) {
            if top_level {
                return Err(ParseError::Error {
                    message: "top level definition must not have a context".to_string(),
                    primary_label: "context starting here".to_string(),
                    primary_label_loc: s2,
                    secondary_labels: vec![("top level definition starts here".to_string(), span)],
                    notes: Vec::new(),
                });
            }
        } else {
            if top_level {
                return Ok(Ast::TopLet {
                    span: start..value.span().end,
                    symbol,
                    args,
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
            }
        }

        let context = parse_expr(lexer)?;
        Ok(Ast::Let {
            span: start..0,
            mutable: false,
            symbol,
            args,
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
) -> Result<(String, Vec<(Option<String>, Monotype)>), ParseError> {
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

        // TODO
        let generics = Vec::new();
        let constraints = Vec::new();

        let mut variants = vec![parse_type_def_variant(lexer, &generics)?];
        while try_token!(lexer, Some((Token::Pipe, _))).is_some() {
            variants.push(parse_type_def_variant(lexer, &generics)?);
        }

        Ok(Ast::DatatypeDefinition {
            span: start..lexer.loc(),
            name,
            generics,
            constraints,
            variants,
        })
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
            match parse_let(lexer, true, &generics, true) {
                Ok(v) => functions.push(v),

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
            match parse_let(lexer, true, &generics, false) {
                Ok(v) => functions.push(v),

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
    match parse_let(lexer, false, &[], false) {
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
    match parse_type_def(lexer) {
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

    match parse_let(lexer, true, &[], false) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e @ Err(_) => return e,
    }

    if let Some((_, span)) = lexer.lex() {
        Err(ParseError::Error {
            message: "unexpected token".to_string(),
            primary_label: "expected `type`, `class`, `instance`, or `let`".to_string(),
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
