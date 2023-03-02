use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Ast {
    Integer(u128),
    Bool(bool),
    Symbol(String),

    Binary {
        op: BinaryOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },

    FuncCall {
        func: Box<Ast>,
        args: Vec<Ast>,
    },

    Let {
        mutable: bool,
        symbol: String,
        value: Box<Ast>,
        context: Box<Ast>,
    },

    If {
        cond: Box<Ast>,
        then: Box<Ast>,
        elsy: Box<Ast>,
    },
}

#[derive(Debug)]
pub enum ParseError {
    NotStarted,
    InvalidValue,
    UnclosedBracket,
    InvalidInfix,
    InvalidLet,
}

macro_rules! try_token {
    ($lexer: expr, $pat: pat) => {{
        let token = $lexer.peek();
        if matches!(token, $pat) {
            $lexer.lex()
        } else {
            None
        }
    }}
}

macro_rules! try_clean {
    ($parse: expr, $lexer: expr, $state: ident, $error: expr) => {
        match $parse {
            Ok(v) => v,
            e @ Err(_) => {
                $lexer.pop($state);
                return e;
            }
        }
    }
}

fn parse_value(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    match lexer.lex() {
        Some(Token::Integer(n)) => Ok(Ast::Integer(n)),
        Some(Token::Bool(b)) => Ok(Ast::Bool(b)),
        Some(Token::Symbol(s)) => Ok(Ast::Symbol(s.to_string())),
        Some(Token::LParen) => {
            let value = parse_top(lexer)?;
            if let Some(Token::RParen) = lexer.lex() {
                Ok(value)
            } else {
                Err(ParseError::UnclosedBracket)
            }
        }

        _ => Err(ParseError::InvalidValue),
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
            func: Box::new(func),
            args,
        })
    }
}

fn parse_muldiv(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_func_call(lexer)?;
    while let Some(token) = try_token!(lexer, Some(Token::Astrisk | Token::Slash)) {
        let right = try_clean!(parse_func_call(lexer), lexer, state, ParseError::InvalidInfix);
        top = Ast::Binary {
            op: match token {
                Token::Astrisk => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
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
    while let Some(token) = try_token!(lexer, Some(Token::Plus | Token::Minus)) {
        let right = try_clean!(parse_muldiv(lexer), lexer, state, ParseError::InvalidInfix);
        top = Ast::Binary {
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

fn parse_let(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some(Token::Let) = lexer.peek() {
        lexer.lex();
        let mutable = matches!(lexer.peek(), Some(Token::Mut));
        if mutable {
            lexer.lex();
        }

        let symbol = match try_token!(lexer, Some(Token::Symbol(_))) {
            Some(Token::Symbol(v)) => v.to_string(),
            _ => return Err(ParseError::InvalidLet),
        };

        if try_token!(lexer, Some(Token::Equals)).is_none() {
            return Err(ParseError::InvalidLet);
        }

        let value = parse_addsub(lexer)?;
        if try_token!(lexer, Some(Token::In)).is_none() {
            return Err(ParseError::InvalidLet);
        }

        let context = parse_top(lexer)?;
        Ok(Ast::Let {
            mutable,
            symbol,
            value: Box::new(value),
            context: Box::new(context),
        })
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_if(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    if let Some(Token::If) = lexer.peek() {
        lexer.lex();
        let cond = parse_addsub(lexer)?;
        if try_token!(lexer, Some(Token::Then)).is_none() {
            return Err(ParseError::InvalidLet);
        }

        let then = parse_top(lexer)?;
        if try_token!(lexer, Some(Token::Else)).is_none() {
            return Err(ParseError::InvalidLet);
        }

        let elsy = parse_top(lexer)?;

        Ok(Ast::If {
            cond: Box::new(cond),
            then: Box::new(then),
            elsy: Box::new(elsy),
        })
    } else {
        Err(ParseError::NotStarted)
    }
}

fn parse_top(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    match parse_let(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e => return e,
    }
    match parse_if(lexer) {
        v @ Ok(_) => return v,
        Err(ParseError::NotStarted) => (),
        e => return e,
    }
    parse_addsub(lexer)
}

pub fn parse(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    parse_top(lexer)
}
