use std::{collections::HashMap, ops::Range};

use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub enum Type {
    Typevar(usize),
    Name(String),
    Generic(String),
    App(Box<Type>, Vec<Type>),
}

#[derive(Debug)]
pub enum Expr {
    Integer(u64),
    Symbol(String),
    FuncCall {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Pattern {
    Wildcard,
    Symbol(String),
    Variant(String, Vec<Pattern>),
    Or(Vec<Pattern>),
}

#[derive(Debug)]
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

    Loop {
        body: Vec<Statement>,
    },

    Break,
    Continue,

    If {
        cond: Expr,
        then: Vec<Statement>,
        elsy: Vec<Statement>,
    },

    Match {
        value: Expr,
        branches: Vec<(Pattern, Vec<Statement>)>,
    },
}

#[derive(Debug)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub result: Type,
}

#[derive(Debug)]
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

macro_rules! consume_token {
    ($lexer: expr, $p: pat, $msg: literal) => {{
        let (token, index, len) = $lexer.peek();
        let $p = token
        else {
            return Err(ParseError {
                range: index..index + len,
                message: $msg.to_owned(),
            });
        };
        $lexer.lex()
    }};
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
                let mut op = None;
                let state = self.lexer.push_state();
                if let (Token::Operator(op_name), ..) = self.lexer.lex() {
                    if let (Token::RParen, ..) = self.lexer.lex() {
                        op = Some(Expr::Symbol(op_name));
                    }
                }

                if let Some(op) = op {
                    op
                } else {
                    self.lexer.pop_state(state);
                    let subexpr = self.parse_expr()?;
                    consume_token!(self.lexer, Token::RParen, "left parenthesis unterminated");
                    subexpr
                }
            }

            Token::Symbol(s) => Expr::Symbol(s),

            Token::Integer(i) => Expr::Integer(i),

            t => return Err(ParseError {
                range: index..index + len,
                message: format!("expected symbol, number, or subexpression; got {:?} instead", t)
            }),
        };

        if matches!(self.lexer.peek(), (Token::LParen, ..)) {
            self.lexer.lex();
            let mut args = Vec::new();
            while !matches!(self.lexer.peek(), (Token::RParen, ..)) {
                args.push(self.parse_expr()?);
                let (Token::Comma, ..) = self.lexer.peek()
                else {
                    break;
                };
                self.lexer.lex();
            }

            consume_token!(self.lexer, Token::RParen, "function call's arguments must be followed by a right parenthesis");
            value = Expr::FuncCall {
                func: Box::new(value),
                args,
            }
        }

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

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_value()?;
        self.parse_expr_tail(left, 0)
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

    fn parse_expr_tail(&mut self, mut left: Expr, min_prec: usize) -> Result<Expr, ParseError> {
        while let Some((op, op_prec, _)) = self.is_infix_of_min_prec(min_prec, true) {
            let mut right = self.parse_value()?;
            while let Some((_, prec, _)) = self.is_infix_of_min_prec(op_prec, false) {
                right = self.parse_expr_tail(right, op_prec + (prec > op_prec) as usize)?;
            }

            left = Expr::FuncCall {
                func: Box::new(Expr::Symbol(op)),
                args: vec![left, right],
            }
        }

        Ok(left)
    }

    fn parse_let(&mut self) -> Result<Statement, ParseError> {
        consume_token!(self.lexer, Token::Let, "let binding must start with `let`");
        let (Token::Symbol(name), ..) = consume_token!(self.lexer, Token::Symbol(_), "let binding currently only supports symbol patterns")
        else {
            unreachable!();
        };
        consume_token!(self.lexer, Token::Equal, "let pattern binding must be followed by `=`");
        let value = self.parse_expr()?;
        Ok(Statement::Let { name, value })
    }

    fn parse_loop(&mut self) -> Result<Statement, ParseError> {
        consume_token!(self.lexer, Token::Loop, "loop must start with `loop`");
        let mut body = Vec::new();
        while !matches!(self.lexer.peek(), (Token::End, ..)) {
            body.push(self.parse_statement()?);
        }

        consume_token!(self.lexer, Token::End, "loop must end with `end`");
        Ok(Statement::Loop { body })
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        consume_token!(self.lexer, Token::If, "if must start with `if`");
        let cond = self.parse_expr()?;

        consume_token!(self.lexer, Token::Then, "if condition must be followed by `then`");
        let mut then = Vec::new();
        while !matches!(self.lexer.peek(), (Token::End | Token::Else, ..)) {
            then.push(self.parse_statement()?);
        }

        let mut elsy = Vec::new();
        if matches!(self.lexer.peek(), (Token::End, ..)) {
            self.lexer.lex();
            Ok(Statement::If { cond, then, elsy })
        } else {
            consume_token!(self.lexer, Token::Else, "then clause of if must be followed by `else` or `end`");
            while !matches!(self.lexer.peek(), (Token::End, ..)) {
                elsy.push(self.parse_statement()?);
            }

            consume_token!(self.lexer, Token::End, "if must end with `end`");
            Ok(Statement::If { cond, then, elsy })
        }
    }

    fn parse_set(&mut self) -> Result<Statement, ParseError> {
        let (Token::Symbol(name), ..) = consume_token!(self.lexer, Token::Symbol(_), "currently only symbols can be set")
        else {
            unreachable!();
        };
        consume_token!(self.lexer, Token::Equal, "pattern being set must be followed by `=`");
        let value = self.parse_expr()?;
        Ok(Statement::Set { name, value })
    }

    fn parse_func_call_stat(&mut self) -> Result<Statement, ParseError> {
        let (Token::Symbol(func), ..) = consume_token!(self.lexer, Token::Symbol(_), "expected function name")
        else {
            unreachable!();
        };

        consume_token!(self.lexer, Token::LParen, "function call must be followed by argument list");
        let mut args = Vec::new();
        while !matches!(self.lexer.peek(), (Token::RParen, ..)) {
            args.push(self.parse_expr()?);
            let (Token::Comma, ..) = self.lexer.peek()
            else {
                break;
            };
            self.lexer.lex();
        }

        consume_token!(self.lexer, Token::RParen, "function call's arguments must be followed by a right parenthesis");

        Ok(Statement::FuncCall {
            func,
            args,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.lexer.peek() {
            (Token::Let, ..) => self.parse_let(),
            (Token::Loop, ..) => self.parse_loop(),
            (Token::Break, ..) => Ok(Statement::Break),
            (Token::Continue, ..) => Ok(Statement::Continue),
            (Token::If, ..) => self.parse_if(),
            _ => {
                let state = self.lexer.push_state();
                if matches!(self.lexer.lex(), (Token::Symbol(_), ..)) {
                    if matches!(self.lexer.peek(), (Token::LParen, ..)) {
                        self.lexer.pop_state(state);
                        self.parse_func_call_stat()
                    } else {
                        self.lexer.pop_state(state);
                        self.parse_set()
                    }
                } else {
                    self.lexer.pop_state(state);
                    self.parse_set()
                }
            }
        }
    }

    fn parse_def(&mut self) -> Result<TopLevel, ParseError> {
        consume_token!(self.lexer, Token::Def, "function declaration starts with `def`");
        let (Token::Symbol(name) | Token::Operator(name), ..) = consume_token!(self.lexer, (Token::Symbol(_) | Token::Operator(_)), "symbol or operator must follow `def`")
        else {
            unreachable!();
        };

        consume_token!(self.lexer, Token::LParen, "function name must be followed by argument list");
        consume_token!(self.lexer, Token::RParen, "function argument list must be followed by a right parenthesis");
        consume_token!(self.lexer, Token::Do, "function body must start with `do`");

        let mut stats = Vec::new();
        while !matches!(self.lexer.peek(), (Token::End, ..)) {
            stats.push(self.parse_statement()?);
        }

        consume_token!(self.lexer, Token::End, "function body must start with `end`");

        Ok(TopLevel::FuncDef {
            name,
            args: Vec::new(),
            ret: Type::Name("unit".to_string()),
            stats,
        })
    }

    pub fn parse(mut self) -> Result<Vec<TopLevel>, ParseError> {
        let mut result = Vec::new();
        while !self.lexer.eof() {
            if let (Token::Declfix, ..) = self.lexer.peek() {
                self.lexer.lex();
                self.lexer.lex();
                if let (Token::Symbol(v), ..) = self.lexer.lex() {
                    if v == "left" || v == "right" {
                        self.lexer.lex();
                    }
                }
            } else if let (Token::Def, ..) = self.lexer.peek() {
                result.push(self.parse_def()?)
            } else {
                let (_, index, len) = self.lexer.peek();
                return Err(ParseError {
                    range: index..index + len,
                    message: "invalid top level construct".to_owned(),
                });
            }
        }

        consume_token!(self.lexer, Token::Eof, "expected eof");
        Ok(result)
    }

    pub fn extract_op_data(&self) -> Vec<(String, OpType)> {
        let mut lexer = self.lexer.clone_clean();
        let mut result = Vec::new();
        loop {
            match lexer.lex() {
                (Token::Eof, ..) => break,
                (Token::Declfix, ..) => {
                    let (Token::Operator(op), ..) = lexer.lex()
                    else {
                        continue;
                    };

                    let (Token::Symbol(v), ..) = lexer.lex()
                    else {
                        continue;
                    };

                    match v.as_str() {
                        "prefix" => result.push((op, OpType::Prefix)),

                        "left" => {
                            let (Token::Integer(prec), ..) = lexer.lex()
                            else {
                                continue;
                            };
                            result.push((op, OpType::Infix(prec as usize, OpAssoc::Left)));
                        }

                        "right" => {
                            let (Token::Integer(prec), ..) = lexer.lex()
                            else {
                                continue;
                            };
                            result.push((op, OpType::Infix(prec as usize, OpAssoc::Right)));
                        }

                        "postfix" => result.push((op, OpType::Prefix)),
                        _ => (),
                    }
                }

                _ => (),
            }
        }

        result
    }
}
