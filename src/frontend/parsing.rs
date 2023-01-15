use std::ops::Range;

#[derive(Debug)]
pub enum Error {
    NumberTooBig,
}

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Int(Range<usize>, u64),
    Char(Range<usize>, u8),
    Float(Range<usize>, f64),
    Str(Range<usize>, String),
    Symbol(Range<usize>, &'a str),
    SymbolOwned(Range<usize>, String),
    Key(Range<usize>, &'a str),

    Quote(Range<usize>, Box<Ast<'a>>),

    SExpr(Range<usize>, Vec<Ast<'a>>),
    Attribute(Range<usize>, Box<Ast<'a>>, Box<Ast<'a>>),
}

impl<'a> PartialEq for Ast<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(_, l1), Self::Int(_, r1)) => l1 == r1,
            (Self::Char(_, l1), Self::Char(_, r1)) => l1 == r1,
            (Self::Float(_, l1), Self::Float(_, r1)) => l1 == r1,
            (Self::Str(_, l1), Self::Str(_, r1)) => l1 == r1,
            (Self::Symbol(_, l1), Self::Symbol(_, r1)) => l1 == r1,
            (Self::SymbolOwned(_, l1), Self::SymbolOwned(_, r1)) => l1 == r1,
            (Self::Key(_, l1), Self::Key(_, r1)) => l1 == r1,
            (Self::Quote(_, l1), Self::Quote(_, r1)) => l1 == r1,
            (Self::SExpr(_, l1), Self::SExpr(_, r1)) => l1 == r1,
            (Self::Attribute(_, l1, l2), Self::Attribute(_, r1, r2)) => l1 == r1 && l2 == r2,
            _ => false,
        }
    }
}

impl<'a> Ast<'a> {
    pub fn span(&self) -> Range<usize> {
        match self {
            Ast::Int(s, _)
            | Ast::Char(s, _)
            | Ast::Float(s, _)
            | Ast::Str(s, _)
            | Ast::Symbol(s, _)
            | Ast::SymbolOwned(s, _)
            | Ast::Key(s, _)
            | Ast::Quote(s, _)
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
