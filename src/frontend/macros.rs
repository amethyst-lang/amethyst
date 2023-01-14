use std::collections::{HashMap, hash_map::Entry};

use super::parsing::Ast;

enum PatternNode<'a> {
    Literal(Ast<'a>),
    SExpr(Vec<PatternNode<'a>>),
    Key(&'a str),
    Symbol(&'a str),
    Attribute(Box<PatternNode<'a>>, Box<PatternNode<'a>>),
    VarPat(Vec<PatternNode<'a>>),
}

impl<'a> PatternNode<'a> {
    fn matches(&self, ast: &Ast<'a>, map: &mut HashMap<String, Ast<'a>>) -> bool {
        match (self, ast) {
            (PatternNode::Literal(a), b) if a == b => true,

            (PatternNode::SExpr(a), Ast::SExpr(_, b)) => {
                for (a, b) in a.iter().zip(b.iter()) {
                    if !a.matches(b, map) {
                        return false;
                    }
                }

                true
            }

            (PatternNode::Key(s), b @ Ast::Key(_, _)) => {
                map.insert((*s).to_owned(), b.clone());
                true
            }

            (PatternNode::Symbol(s), b) => {
                map.insert((*s).to_owned(), b.clone());
                true
            }

            (PatternNode::Attribute(v1, a1), Ast::Attribute(_, v2, a2)) => {
                v1.matches(v2, map) | a1.matches(a2, map)
            }

            (PatternNode::VarPat(_), _) => todo!(),

            _ => false,
        }
    }
}

enum ReplacementNode<'a> {
    Literal(Ast<'a>),
    SExpr(Vec<ReplacementNode<'a>>),
    Symbol(&'a str),
    Attribute(Box<ReplacementNode<'a>>, Box<ReplacementNode<'a>>),
    VarPat(Vec<ReplacementNode<'a>>),
}

impl<'a> ReplacementNode<'a> {
    fn replace(&self, map: &HashMap<String, Ast<'a>>) -> Ast<'a> {
        match self {
            ReplacementNode::Literal(v) => v.clone(),

            ReplacementNode::SExpr(v) => Ast::SExpr(0..0, v.iter().map(|v| v.replace(map)).collect()),

            ReplacementNode::Symbol(v) => {
                if let Some(v) = map.get(*v) {
                    v.clone()
                } else {
                    Ast::Symbol(0..0, *v)
                }
            }

            ReplacementNode::Attribute(v, a) => Ast::Attribute(0..0, Box::new(v.replace(map)), Box::new(a.replace(map))),

            ReplacementNode::VarPat(_) => todo!(),
        }
    }
}

struct Macros<'a> {
    pat_rep_pairs: Vec<(PatternNode<'a>, ReplacementNode<'a>)>,
}

fn extract_pattern<'a>(ast: &Ast<'a>) -> PatternNode<'a> {
    match ast {
        Ast::Int(_, _)
        | Ast::Char(_, _)
        | Ast::Float(_, _)
        | Ast::Str(_, _) => PatternNode::Literal(ast.clone()),

        Ast::Key(_, s) => PatternNode::Key(*s),
        Ast::Symbol(_, s) => PatternNode::Symbol(*s),
        Ast::Quote(_, v) => PatternNode::Literal((**v).clone()),

        Ast::SExpr(_, v) => {
            if !v.is_empty() && matches!(v[0], Ast::Symbol(_, "..")) {
                PatternNode::VarPat(v[1..].iter().map(extract_pattern).collect())
            } else {
                PatternNode::SExpr(v.iter().map(extract_pattern).collect())
            }
        }

        Ast::Attribute(_, v, a) => PatternNode::Attribute(Box::new(extract_pattern(&**v)), Box::new(extract_pattern(&**a))),
    }
}

fn extract_replacement<'a>(ast: &Ast<'a>) -> ReplacementNode<'a> {
    match ast {
        Ast::Int(_, _)
        | Ast::Char(_, _)
        | Ast::Float(_, _)
        | Ast::Str(_, _)
        | Ast::Key(_, _) => ReplacementNode::Literal(ast.clone()),

        Ast::Symbol(_, s) => ReplacementNode::Symbol(*s),
        Ast::Quote(_, v) => ReplacementNode::Literal((**v).clone()),

        Ast::SExpr(_, v) => {
            if !v.is_empty() && matches!(v[0], Ast::Symbol(_, "..")) {
                ReplacementNode::VarPat(v[1..].iter().map(extract_replacement).collect())
            } else {
                ReplacementNode::SExpr(v.iter().map(extract_replacement).collect())
            }
        }

        Ast::Attribute(_, v, a) => ReplacementNode::Attribute(Box::new(extract_replacement(&**v)), Box::new(extract_replacement(&**a))),
    }
}

fn find_macros<'a>(ast: &Ast<'a>, macros: &mut HashMap<String, Macros<'a>>) {
    match ast {
        Ast::Quote(_, v) => find_macros(v, macros),

        Ast::SExpr(_, v) => {
            if v.len() >= 3 && matches!(v.get(0), Some(Ast::Symbol(_, "defmacro"))) {
                let (name, pattern) = match &v[1] {
                    Ast::SExpr(_, v) if !v.is_empty() => {
                        let name = match v[0] {
                            Ast::Symbol(_, n) => n,
                            _ => return,
                        };
                        let pattern = PatternNode::SExpr(v.iter().map(extract_pattern).collect());
                        (name, pattern)
                    }
                    _ => return,
                };

                let replacement = ReplacementNode::SExpr([ReplacementNode::Literal(Ast::Symbol(0..0, "seq"))].into_iter().chain(v[2..].iter().map(extract_replacement)).collect());
                match macros.entry(name.to_owned()) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().pat_rep_pairs.push((pattern, replacement));
                    }

                    Entry::Vacant(v) => {
                        v.insert(Macros {
                            pat_rep_pairs: vec![(pattern, replacement)],
                        });
                    }
                }
            } else {
                for v in v {
                    find_macros(v, macros);
                }
            }
        }

        Ast::Attribute(_, v, a) => {
            find_macros(v, macros);
            find_macros(a, macros);
        }

        _ => (),
    }
}

fn apply_macros<'a>(ast: &mut Ast<'a>, macros: &HashMap<String, Macros<'a>>) -> bool {
    match ast {
        Ast::Quote(_, v) => {
            apply_macros(v, macros)
        }

        Ast::SExpr(_, v) if !v.is_empty() && !matches!(v[0], Ast::Symbol(_, "defmacro")) => {
            match v.get(0) {
                Some(Ast::Symbol(_, macro_name)) if macros.contains_key(*macro_name) => {
                    if let Some(macro_) = macros.get(*macro_name) {
                        for (pat, rep) in macro_.pat_rep_pairs.iter().rev() {
                            let mut map = HashMap::new();
                            if pat.matches(ast, &mut map) {
                                *ast = rep.replace(&map);
                                return true;
                            }
                        }

                        false
                    } else {
                        false
                    }
                }

                _ => {
                    let mut b = false;
                    for v in v {
                        b |= apply_macros(v, macros);
                    }
                    b
                }
            }
        }

        Ast::Attribute(_, v, a) => {
            apply_macros(v, macros) | apply_macros(a, macros)
        }

        _ => false,
    }
}

pub fn execute_macros(asts: &mut [Ast<'_>]) {
    let mut changed = true;
    let mut macros = HashMap::new();
    while changed {
        changed = false;
        macros.clear();

        for ast in asts.iter() {
            find_macros(ast, &mut macros);
        }

        for ast in asts.iter_mut() {
            changed |= apply_macros(ast, &macros);
        }
    }
}
