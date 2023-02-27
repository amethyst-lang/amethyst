#[derive(Copy, Clone, Debug)]
pub enum Token<'a> {
    Invalid(&'a str),
    Number(u128),
    // Bool(bool),
    Plus,
    Minus,
    Astrisk,
    Slash,
}

pub struct Lexer<'a> {
    contents: &'a str,
    prev_tokens: Vec<Token<'a>>,
    token_index: usize,
    pos: usize,
}

#[derive(Copy, Clone)]
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

    pub fn lex(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.prev_tokens.get(self.token_index) {
            self.token_index += 1;
            Some(*token)
        } else {
            let mut final_pos = self.pos;

            enum State {
                Initial,
                Invalid,
                Number,
                SingleChar,
            }

            let mut state = State::Initial;
            for c in self.contents[self.pos..].chars() {
                match state {
                    State::Initial => {
                        match c {
                            '0'..='9' => state = State::Number,
                            '+' | '-' | '*' | '/' => state = State::SingleChar,
                            ' ' | '\t' | '\n' | '\r' => self.pos += c.len_utf8(),
                            _ => state = State::Invalid,
                        }
                    }

                    State::Invalid => break,

                    State::Number => {
                        match c {
                            '0'..='9' => (),
                            _ => break,
                        }
                    }

                    State::SingleChar => break,
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
                State::Initial => unreachable!(),
                State::Invalid => Token::Invalid(s),
                State::Number => Token::Number(s.parse().unwrap()),
                State::SingleChar => match s {
                    "+" => Token::Plus,
                    "-" => Token::Minus,
                    "*" => Token::Astrisk,
                    "/" => Token::Slash,
                    _ => unreachable!(),
                },
            };

            self.prev_tokens.push(token);
            self.token_index += 1;
            Some(token)
        }
    }
}
