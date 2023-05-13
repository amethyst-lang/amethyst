use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Copy, Clone, Debug)]
pub enum Token<'a> {
    Invalid(&'a str),
    Integer(u128),
    Bool(bool),
    Plus,
    Minus,
    Astrisk,
    Slash,
    Percent,
    Equals,
    Pipe,
    Colon,
    Arrow,
    At,
    Comma,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    LogicalAnd,
    LogicalOr,
    Exclamation,
    Let,
    In,
    Mut,
    If,
    Then,
    Else,
    Match,
    With,
    To,
    End,
    Forall,
    Type,
    Class,
    Instance,
    Where,
    Symbol(&'a str),
}

pub struct Lexer<'a> {
    contents: &'a str,
    prev_tokens: Vec<(Token<'a>, Span)>,
    token_index: usize,
    pos: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct LexerState {
    token_index: usize,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self {
            contents,
            prev_tokens: Vec::new(),
            token_index: 0,
            pos: 0,
        }
    }

    pub fn loc(&self) -> usize {
        self.pos
    }

    pub fn push(&self) -> LexerState {
        LexerState {
            token_index: self.token_index,
            pos: self.pos,
        }
    }

    pub fn pop(&mut self, state: LexerState) {
        self.token_index = state.token_index;
        self.pos = state.pos;
    }

    pub fn peek(&mut self) -> Option<(Token<'a>, Span)> {
        let state = self.push();
        let token = self.lex();
        self.pop(state);
        token
    }

    pub fn lex(&mut self) -> Option<(Token<'a>, Span)> {
        if let Some((token, span)) = self.prev_tokens.get(self.token_index) {
            self.token_index += 1;
            self.pos = span.end;
            Some((*token, span.clone()))
        } else {
            let mut final_pos = self.pos;

            enum State {
                Initial,
                Invalid,
                Number,
                SingleChar,
                Symbol,
                Minus,
                AppendEq,
                Not,
                Double,
                Slash,
                SingleComment,
            }

            let mut state = State::Initial;
            for c in self.contents[self.pos..].chars() {
                match state {
                    State::Initial => match c {
                        '0'..='9' => state = State::Number,
                        '+' | '*' | '%' | '(' | ')' | '[' | ']' | '{' | '}' | ':' | '@' | ',' => {
                            state = State::SingleChar
                        }
                        '/' => state = State::Slash,
                        '|' | '&' => state = State::Double,
                        '<' | '>' | '=' => state = State::AppendEq,
                        '!' => state = State::Not,
                        '-' => state = State::Minus,
                        ' ' | '\t' | '\n' | '\r' => self.pos += c.len_utf8(),
                        'a'..='z' | 'A'..='Z' | '_' => state = State::Symbol,
                        _ => state = State::Invalid,
                    },

                    State::Invalid => break,

                    State::Number => match c {
                        '0'..='9' => (),
                        _ => break,
                    },

                    State::SingleChar => break,

                    State::Symbol => match c {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => (),
                        _ => break,
                    },

                    State::Minus => match c {
                        '>' => state = State::SingleChar,
                        _ => break,
                    },

                    State::AppendEq => {
                        state = State::SingleChar;
                        if c != '=' {
                            break;
                        }
                    }

                    State::Not => match c {
                        '=' => state = State::SingleChar,
                        _ => break,
                    },

                    State::Double => {
                        state = State::SingleChar;
                        if c != self.contents[self.pos..].chars().next().unwrap() {
                            break;
                        }
                    }

                    State::Slash => {
                        if c == '/' {
                            self.pos += c.len_utf8() * 2;
                            state = State::SingleComment;
                        } else {
                            break;
                        }
                    }

                    State::SingleComment => {
                        if c == '\n' {
                            state = State::Initial;
                        }
                        self.pos += c.len_utf8();
                    }
                }

                final_pos += c.len_utf8();
            }

            let initial_pos = self.pos;
            self.pos = final_pos;

            if initial_pos == final_pos {
                return None;
            }

            let s = &self.contents[initial_pos..final_pos];
            let token = match state {
                State::Initial | State::AppendEq | State::Double | State::SingleComment => {
                    unreachable!()
                }
                State::Invalid => Token::Invalid(s),
                State::Number => Token::Integer(s.parse().unwrap()),
                State::Not => Token::Exclamation,
                State::SingleChar => match s {
                    "+" => Token::Plus,
                    "-" => Token::Minus,
                    "*" => Token::Astrisk,
                    "/" => Token::Slash,
                    "%" => Token::Percent,
                    "(" => Token::LParen,
                    ")" => Token::RParen,
                    "[" => Token::LBrack,
                    "]" => Token::RBrack,
                    "{" => Token::LBrace,
                    "}" => Token::RBrace,
                    "|" => Token::Pipe,
                    ":" => Token::Colon,
                    "->" => Token::Arrow,
                    "@" => Token::At,
                    "," => Token::Comma,
                    "=" => Token::Equals,
                    "==" => Token::Eq,
                    ">=" => Token::Ge,
                    "<=" => Token::Le,
                    ">" => Token::Gt,
                    "<" => Token::Lt,
                    "!=" => Token::Ne,
                    "&" => Token::Invalid(s),
                    "&&" => Token::LogicalAnd,
                    "||" => Token::LogicalOr,
                    _ => unreachable!("{:?}", s),
                },
                State::Symbol => match s {
                    "true" | "false" => Token::Bool(s == "true"),
                    "let" => Token::Let,
                    "in" => Token::In,
                    "mut" => Token::Mut,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "match" => Token::Match,
                    "with" => Token::With,
                    "to" => Token::To,
                    "end" => Token::End,
                    "forall" => Token::Forall,
                    "type" => Token::Type,
                    "class" => Token::Class,
                    "instance" => Token::Instance,
                    "where" => Token::Where,
                    _ => Token::Symbol(s),
                },
                State::Minus => Token::Minus,
                State::Slash => Token::Slash,
            };

            self.prev_tokens.push((token, initial_pos..final_pos));
            self.token_index += 1;
            Some((token, initial_pos..final_pos))
        }
    }
}
