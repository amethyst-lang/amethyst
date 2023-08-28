#[derive(Debug, Clone)]
pub enum Token {
    Invalid,
    Eof,
    LParen,
    RParen,
    Comma,
    Equal,
    Symbol(String),
    Operator(String),
    Integer(u64),
    Def,
    Do,
    End,
    Let,
    Loop,
    Break,
    Continue,
    If,
    Then,
    Else,
}

pub struct Lexer {
    string: String,
    index: usize,
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
        }

        let mut len = 0;
        let mut state = State::Start;
        for c in self.string[self.index..].chars() {
            match state {
                State::Start => {
                    match c {
                        'a'..='z' | 'A'..='Z' | '_' => state = State::Symbol,
                        '0'..='9' => state = State::Number,
                        '(' | ')' => state = State::SingleChar,

                        ' ' | '\t' | '\n' | '\r' => {
                            self.index += c.len_utf8();
                            continue;
                        }

                        '#' => {
                            state = State::Comment;
                            self.index += c.len_utf8();
                            continue;
                        }

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
            }

            len += c.len_utf8();
        }

        let index = self.index;
        self.index += len;
        let token = match state {
            State::Start | State::Comment => Token::Eof,
            State::Invalid => Token::Invalid,

            State::Symbol => {
                match &self.string[index..index + len] {
                    "def" => Token::Def,
                    "do" => Token::Do,
                    "end" => Token::End,
                    "let" => Token::Let,
                    "loop" => Token::Loop,
                    "break" => Token::Break,
                    "continue" => Token::Continue,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    s => Token::Symbol(s.to_owned()),
                }
            }

            State::Number => Token::Integer(self.string[index..index + len].parse().unwrap()),

            State::Operator => {
                match &self.string[index..index + len] {
                    "," => Token::Comma,
                    "=" => Token::Equal,
                    s => Token::Operator(s.to_owned())
                }
            }

            State::SingleChar => {
                match &self.string[index..index + len] {
                    "(" => Token::LParen,
                    ")" => Token::RParen,
                    _ => unreachable!(),
                }
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
