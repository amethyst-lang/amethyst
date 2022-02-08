#[cfg(not(feature = "std"))]
use crate::*;

#[derive(Debug)]
pub enum Error {
    NumberTooBig,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast<'a> {
    Int(u64),
    Float(f64),
    Str(String),
    Symbol(&'a str),
    Key(&'a str),

    Quote(Box<Ast<'a>>),
    Comma(Box<Ast<'a>>),
    Backtick(Box<Ast<'a>>),

    SExpr(Vec<Ast<'a>>),
    Attribute(Vec<Ast<'a>>),
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
