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
    //Bool(bool),
    Binary {
        op: BinaryOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },
}

#[derive(Debug)]
pub enum ParseError {
    InvalidValue,
    UnclosedBracket,
    InvalidInfix,
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
        Some(Token::LParen) => {
            let value = parse_addsub(lexer)?;
            if let Some(Token::RParen) = lexer.lex() {
                Ok(value)
            } else {
                Err(ParseError::UnclosedBracket)
            }
        }

        _ => Err(ParseError::InvalidValue),
    }
}

fn parse_muldiv(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    let state = lexer.push();
    let mut top = parse_value(lexer)?;
    while let Some(token) = try_token!(lexer, Some(Token::Astrisk | Token::Slash)) {
        let right = try_clean!(parse_value(lexer), lexer, state, ParseError::InvalidInfix);
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

pub fn parse(lexer: &mut Lexer<'_>) -> Result<Ast, ParseError> {
    parse_addsub(lexer)
}
