#[derive(Debug, Clone)]
pub enum Token {
    Invalid,
    Eof,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Comma,
    Equal,
    Colon,
    RArrow,
    Symbol(String),
    Operator(String),
    String(String),
    Integer(u64),
    Declfix,
    Def,
    Do,
    End,
    Let,
    Loop,
    Break,
    Continue,
    Return,
    If,
    Then,
    Else,
    Type,
    As,
    Match,
    To,
    OrPat,
    RangePat,
    WildcardPat,
}

pub enum LexerContext {
    Normal,
    Pattern,
}

pub struct Lexer {
    string: String,
    index: usize,
    pub context: LexerContext,
}

pub struct LexerState(usize);

fn valid_operator_char(c: char) -> bool {
    "`~!@$%^&*-+=|;:,<.>/?".contains(c)
}

impl Lexer {
    pub fn new(string: &str) -> Self {
        Lexer {
            string: string.to_owned(),
            index: 0,
            context: LexerContext::Normal,
        }
    }

    pub fn clone_clean(&self) -> Self {
        Lexer {
            string: self.string.clone(),
            index: 0,
            context: LexerContext::Normal,
        }
    }

    pub fn push_state(&self) -> LexerState {
        LexerState(self.index)
    }

    pub fn pop_state(&mut self, state: LexerState) {
        self.index = state.0;
    }

    pub fn lex(&mut self) -> (Token, usize, usize) {
        enum State {
            Start,
            Invalid,
            Comment,
            Symbol,
            Number,
            Operator,
            SingleChar,
            String(bool),
            StringEnd,
        }

        let mut len = 0;
        let mut state = State::Start;
        for c in self.string[self.index..].chars() {
            match state {
                State::Start => {
                    match c {
                        'a'..='z' | 'A'..='Z' | '_' => state = State::Symbol,
                        '0'..='9' => state = State::Number,
                        '(' | ')' | '[' | ']' => state = State::SingleChar,

                        ' ' | '\t' | '\n' | '\r' => {
                            self.index += c.len_utf8();
                            continue;
                        }

                        '#' => {
                            state = State::Comment;
                            self.index += c.len_utf8();
                            continue;
                        }

                        '"' => state = State::String(false),

                        _ if valid_operator_char(c) => state = State::Operator,
                        _ => state = State::Invalid,
                    }
                }

                State::Invalid => break,

                State::Comment => {
                    self.index += c.len_utf8();
                    if c == '\n' {
                        state = State::Start;
                    }

                    continue;
                }

                State::Symbol => {
                    if !('a'..='z').contains(&c) && !('A'..='Z').contains(&c) && !('0'..='9').contains(&c) && c != '_' {
                        break;
                    }
                }

                State::Number => {
                    if !('0'..='9').contains(&c) {
                        break;
                    }
                }

                State::Operator => {
                    if !valid_operator_char(c) {
                        break;
                    }
                }

                State::SingleChar => break,

                State::String(true) => {
                    if c == 'n' || c == 'r' || c == 't' || c == '\'' || c == '"' {
                        state = State::String(false);
                    } else {
                        state = State::Invalid;
                    }
                }

                State::String(false) => {
                    if c == '"' {
                        state = State::StringEnd;
                    } else if c == '\\' {
                        state = State::String(true);
                    }
                }

                State::StringEnd => break,
            }

            len += c.len_utf8();
        }

        let index = self.index;
        self.index += len;
        let token = match state {
            State::Start | State::Comment => Token::Eof,
            State::Invalid | State::String(_) => Token::Invalid,

            State::Symbol => {
                match &self.string[index..index + len] {
                    "declfix" => Token::Declfix,
                    "def" => Token::Def,
                    "do" => Token::Do,
                    "end" => Token::End,
                    "let" => Token::Let,
                    "loop" => Token::Loop,
                    "break" => Token::Break,
                    "continue" => Token::Continue,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "type" => Token::Type,
                    "as" => Token::As,
                    "match" => Token::Match,
                    "to" => Token::To,
                    "_" if matches!(self.context, LexerContext::Pattern) => Token::WildcardPat,
                    s => Token::Symbol(s.to_owned()),
                }
            }

            State::Number => Token::Integer(self.string[index..index + len].parse().unwrap()),

            State::Operator => {
                match &self.string[index..index + len] {
                    "," => Token::Comma,
                    "=" => Token::Equal,
                    ":" => Token::Colon,
                    "->" => Token::RArrow,
                    "|" if matches!(self.context, LexerContext::Pattern) => Token::OrPat,
                    ".." if matches!(self.context, LexerContext::Pattern) => Token::RangePat,
                    s => Token::Operator(s.to_owned())
                }
            }

            State::SingleChar => {
                match &self.string[index..index + len] {
                    "(" => Token::LParen,
                    ")" => Token::RParen,
                    "[" => Token::LBrack,
                    "]" => Token::RBrack,
                    _ => unreachable!(),
                }
            }

            State::StringEnd => {
                let mut s = String::new();
                let mut backslash = false;
                for c in self.string[index + 1..index + len - 1].chars() {
                    if backslash {
                        s.push(match c {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '"' => '\"',
                            '\'' => '\'',
                            _ => unreachable!("already checked"),
                        });
                        backslash = false;
                    } else if c == '\\' {
                        backslash = true;
                    } else {
                        s.push(c);
                    }
                }

                Token::String(s)
            }
        };

        (token, index, len)
    }

    pub fn peek(&mut self) -> (Token, usize, usize) {
        let state = self.push_state();
        let result = self.lex();
        self.pop_state(state);
        result
    }

    pub fn eof(&self) -> bool {
        self.index >= self.string.len()
    }
}
