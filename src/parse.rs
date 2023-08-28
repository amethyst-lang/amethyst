use std::{collections::HashMap, ops::Range};

use crate::lexer::{Lexer, Token};

pub enum Type {
    Typevar(usize),
    Name(String),
    Generic(String),
    App(Box<Type>, Vec<Type>),
}

#[derive(Debug)]
pub enum Expr {
    Integer(u64),
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

#[derive(Debug)]
pub struct ParseError {
    pub range: Range<usize>,
    pub message: String,
}

#[derive(Clone, Copy)]
pub enum OpAssoc {
    Left,
    Right
}

#[derive(Clone, Copy)]
pub enum OpType {
    Prefix,
    Infix(usize, OpAssoc),
    Postfix,
}

pub struct Parser {
    lexer: Lexer,
    pub op_data: HashMap<String, OpType>,
}

impl Parser {
    pub fn new(string: &str) -> Self {
        Parser {
            lexer: Lexer::new(string),
            op_data: HashMap::new(),
        }
    }

    pub fn default_op_data() -> HashMap<String, OpType> {
        use OpType::*;
        use OpAssoc::*;

        let mut map = HashMap::new();
        map.insert("::".to_owned(), Infix(0, Right));
        map.insert("+".to_owned(), Infix(10, Left));
        map.insert("-".to_owned(), Infix(10, Left));
        map.insert("*".to_owned(), Infix(20, Left));
        map.insert("/".to_owned(), Infix(20, Left));
        map.insert("**".to_owned(), Infix(30, Right));
        map.insert("?".to_owned(), Postfix);
        map.insert(".*".to_owned(), Postfix);
        map.insert("~".to_owned(), Prefix);
        map.insert("!".to_owned(), Prefix);
        map
    }

    fn parse_value(&mut self) -> Result<Expr, ParseError> {
        let mut prefixes = Vec::new();
        while let (Token::Operator(op), _, _) = self.lexer.peek() {
            let Some(OpType::Prefix) = self.op_data.get(&op)
            else {
                break;
            };
            prefixes.push(op);
            self.lexer.lex();
        }

        let (token, index, len) = self.lexer.lex();
        let mut value = match token {
            Token::LParen => {
                let subexpr = self.parse_infix()?;
                let (Token::RParen, _, _) = self.lexer.lex()
                else {
                    return Err(ParseError {
                        range: index..index + len,
                        message: "left parenthesis unterminated".to_string(),
                    });
                };
                subexpr
            }

            Token::Symbol(s) => Expr::Symbol(s),

            Token::Integer(i) => Expr::Integer(i),

            t => return Err(ParseError {
                range: index..index + len,
                message: format!("expected symbol, number, or subexpression; got {:?} instead", t)
            }),
        };


        while let (Token::Operator(op), _, _) = self.lexer.peek() {
            let Some(OpType::Postfix) = self.op_data.get(&op)
            else {
                break;
            };
            value = Expr::FuncCall {
                func: Box::new(Expr::Symbol(op)),
                args: vec![value],
            };
            self.lexer.lex();
        }

        for op in prefixes.into_iter().rev() {
            value = Expr::FuncCall {
                func: Box::new(Expr::Symbol(op)),
                args: vec![value],
            };
        }

        Ok(value)
    }

    fn parse_infix(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_value()?;
        self.parse_infix_tail(left, 0)
    }

    fn is_infix_of_min_prec(&mut self, min_prec: usize, can_eq: bool) -> Option<(String, usize, OpAssoc)> {
        let (Token::Operator(lookahead), _, _) = self.lexer.peek()
        else {
            return None;
        };
        let Some(OpType::Infix(prec, assoc)) = self.op_data.get(&lookahead)
        else {
            return None;
        };

        let prec = *prec;
        let assoc = *assoc;
        if can_eq && prec >= min_prec {
            self.lexer.lex();
            Some((lookahead, prec, assoc))
        } else if prec > min_prec || matches!(assoc, OpAssoc::Right) && prec == min_prec {
            Some((lookahead, prec, assoc))
        } else {
            None
        }
    }

    fn parse_infix_tail(&mut self, mut left: Expr, min_prec: usize) -> Result<Expr, ParseError> {
        while let Some((op, op_prec, _)) = self.is_infix_of_min_prec(min_prec, true) {
            let mut right = self.parse_value()?;
            while let Some((_, prec, _)) = self.is_infix_of_min_prec(op_prec, false) {
                right = self.parse_infix_tail(right, op_prec + (prec > op_prec) as usize)?;
            }

            left = Expr::FuncCall {
                func: Box::new(Expr::Symbol(op)),
                args: vec![left, right],
            }
        }

        Ok(left)
    }

    pub fn parse(mut self) -> Result<Expr, ParseError> {
        self.parse_infix()
    }
}
