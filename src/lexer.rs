use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Copy, Clone, Debug)]
pub enum Token<'a> {
    Invalid(&'a str),
    Integer(u128),
    Bool(bool),
    Equals,
    Colon,
    Arrow,
    Dot,
    Comma,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Def,
    Do,
    End,
    Let,
    If,
    Then,
    Else,
    Match,
    As,
    To,
    Type,
    Operator(&'a str, bool, i32),
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

            enum State<'a> {
                Initial,
                Invalid,
                Number,
                Symbol,
                SingleComment,
                Done(Token<'a>),
            }

            let mut state = State::Initial;
            for c in self.contents[self.pos..].chars() {
                match state {
                    State::Initial => match c {
                        '0'..='9' => state = State::Number,
                        '#' => state = State::SingleComment,
                        ' ' | '\t' | '\n' | '\r' => self.pos += c.len_utf8(),
                        'a'..='z' | 'A'..='Z' | '_' => state = State::Symbol,
                        _ => state = State::Invalid,
                    },

                    State::Invalid => break,

                    State::Number => match c {
                        '0'..='9' => (),
                        _ => break,
                    },

                    State::Symbol => match c {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => (),
                        _ => break,
                    },

                    State::SingleComment => {
                        if c == '\n' {
                            state = State::Initial;
                        }
                        self.pos += c.len_utf8();
                    }

                    State::Done(_) => break,
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
                State::Initial | State::SingleComment => {
                    unreachable!()
                }

                State::Invalid => Token::Invalid(s),
                State::Number => Token::Integer(s.parse().unwrap()),
                State::Symbol => match s {
                    "true" | "false" => Token::Bool(s == "true"),
                    "let" => Token::Let,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "match" => Token::Match,
                    "to" => Token::To,
                    "end" => Token::End,
                    "type" => Token::Type,
                    _ => Token::Symbol(s),
                },

                State::Done(token) => token,
            };

            self.prev_tokens.push((token, initial_pos..final_pos));
            self.token_index += 1;
            Some((token, initial_pos..final_pos))
        }
    }
}
