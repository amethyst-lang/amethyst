use std::ops::Range;

#[derive(Debug)]
pub enum Error {
    NumberTooBig,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast<'a> {
    Int(Range<usize>, u64),
    Char(Range<usize>, u8),
    Float(Range<usize>, f64),
    Str(Range<usize>, String),
    Symbol(Range<usize>, &'a str),
    Key(Range<usize>, &'a str),

    Quote(Range<usize>, Box<Ast<'a>>),
    Comma(Range<usize>, Box<Ast<'a>>),
    Backtick(Range<usize>, Box<Ast<'a>>),
    Splice(Range<usize>, Box<Ast<'a>>),

    SExpr(Range<usize>, Vec<Ast<'a>>),
    Attribute(Range<usize>, Box<Ast<'a>>, Box<Ast<'a>>),
}

impl<'a> Ast<'a> {
    pub fn span(&self) -> Range<usize> {
        match self {
            Ast::Int(s, _)
            | Ast::Char(s, _)
            | Ast::Float(s, _)
            | Ast::Str(s, _)
            | Ast::Symbol(s, _)
            | Ast::Key(s, _)
            | Ast::Quote(s, _)
            | Ast::Comma(s, _)
            | Ast::Backtick(s, _)
            | Ast::Splice(s, _)
            | Ast::SExpr(s, _)
            | Ast::Attribute(s, _, _) => s.clone(),
        }
    }
}

pub fn unescape_sequences(s: &str) -> String {
    let mut iter = s.chars();
    let mut s = String::new();

    while let Some(c) = iter.next() {
        if c == '\\' {
            match iter.next().unwrap() {
                '\\' => s.push('\\'),
                '\"' => s.push('\"'),
                '\'' => s.push('\''),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                '0' => s.push('\0'),
                c => {
                    s.push('\\');
                    s.push(c)
                }
            };
        } else {
            s.push(c);
        }
    }

    s
}

pub fn strip_backslashes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());

    for line in s.split('\n') {
        if let Some(stripped) = line.trim_start().strip_prefix("\\\\") {
            result.push_str(stripped);
            result.push('\n');
        }
    }

    result
}

#[cfg(test)]
mod test {
    #[test]
    fn parsing() {
        use crate::parser::TopParser;
        assert!(TopParser::new()
            .parse(r#"(+ 1 2 "uwu" (3 4 5) a.(b c).d.e) (a b c)"#)
            .is_ok());
    }
}
