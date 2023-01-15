use std::collections::{hash_map::Entry, HashMap};

use super::parsing::Ast;

enum PatternNode<'a> {
    Literal(Ast<'a>),
    SExpr(Vec<PatternNode<'a>>),
    Symbol(&'a str),
    Attribute(Box<PatternNode<'a>>, Box<PatternNode<'a>>),
    VarPat(Vec<PatternNode<'a>>),
}

impl<'a> PatternNode<'a> {
    fn matches(&self, ast: &Ast<'a>, map: &mut HashMap<String, Vec<Ast<'a>>>) -> bool {
        match (self, ast) {
            (PatternNode::Literal(a), b) if a == b => true,

            (PatternNode::SExpr(a), Ast::SExpr(_, b)) => {
                let mut i = 0;
                let mut j = 0;
                let mut in_var_pat = false;
                'a: while i < a.len() && j < b.len() {
                    if in_var_pat {
                        let a = &a[i];
                        if let PatternNode::VarPat(v) = a {
                            let mut offset = 0;
                            for v in v {
                                match b.get(j + offset) {
                                    Some(b) => {
                                        if !v.matches(b, map) {
                                            in_var_pat = false;
                                            i += 1;
                                            continue 'a;
                                        }
                                    }

                                    None => {
                                        in_var_pat = false;
                                        i += 1;
                                        continue 'a;
                                    }
                                }

                                offset += 1;
                            }

                            j += offset;
                        } else {
                            unreachable!()
                        }
                    } else {
                        let a = &a[i];
                        let b = &b[j];
                        if let PatternNode::VarPat(_) = a {
                            in_var_pat = true;
                            continue;
                        }
                        if !a.matches(b, map) {
                            return false;
                        }
                        i += 1;
                        j += 1;
                    }
                }

                if in_var_pat {
                    i += 1;
                }

                i >= a.len() && j >= b.len()
            }

            (PatternNode::Symbol(s), b) => {
                match map.entry(s.to_string()) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push(b.clone());
                    }

                    Entry::Vacant(v) => {
                        v.insert(vec![b.clone()]);
                    }
                }

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
    fn replace(&self, map: &HashMap<String, Vec<Ast<'a>>>) -> Ast<'a> {
        match self {
            ReplacementNode::Literal(v) => v.clone(),

            ReplacementNode::SExpr(v) => {
                let mut vec = Vec::new();

                for v in v {
                    if let ReplacementNode::VarPat(v) = v {
                        let mut cont = true;
                        let mut count = 0;
                        while cont {
                            let values: Vec<_> = v
                                .iter()
                                .map(|v| v.replace_within_varpat(map, count, &mut cont))
                                .collect();
                            if cont {
                                vec.extend(values);
                            }
                            count += 1;
                        }
                    } else {
                        vec.push(v.replace(map))
                    }
                }

                Ast::SExpr(0..0, vec)
            }

            ReplacementNode::Symbol(v) => {
                if let Some(v) = map.get(*v) {
                    if v.len() == 1 {
                        v[0].clone()
                    } else {
                        Ast::SExpr(0..0, Vec::new())
                    }
                } else {
                    Ast::Symbol(0..0, *v)
                }
            }

            ReplacementNode::Attribute(v, a) => {
                Ast::Attribute(0..0, Box::new(v.replace(map)), Box::new(a.replace(map)))
            }

            ReplacementNode::VarPat(v) => {
                let mut cont = true;
                let mut count = 0;
                let mut vec = Vec::new();
                while cont {
                    let values: Vec<_> = v
                        .iter()
                        .map(|v| v.replace_within_varpat(map, count, &mut cont))
                        .collect();
                    if cont {
                        vec.extend(values);
                    }
                    count += 1;
                }

                Ast::SExpr(0..0, vec)
            }
        }
    }

    fn replace_within_varpat(
        &self,
        map: &HashMap<String, Vec<Ast<'a>>>,
        count: usize,
        cont: &mut bool,
    ) -> Ast<'a> {
        match self {
            ReplacementNode::Literal(v) => v.clone(),

            ReplacementNode::SExpr(v) => Ast::SExpr(
                0..0,
                v.iter()
                    .map(|v| v.replace_within_varpat(map, count, cont))
                    .collect(),
            ),

            ReplacementNode::Symbol("#count") => Ast::Int(0..0, count as u64),

            ReplacementNode::Symbol(v) => {
                if let Some(v) = map.get(*v) {
                    if count < v.len() {
                        v[count].clone()
                    } else {
                        *cont = false;
                        Ast::SExpr(0..0, Vec::new())
                    }
                } else {
                    Ast::Symbol(0..0, *v)
                }
            }

            ReplacementNode::Attribute(v, a) => Ast::Attribute(
                0..0,
                Box::new(v.replace_within_varpat(map, count, cont)),
                Box::new(a.replace_within_varpat(map, count, cont)),
            ),

            ReplacementNode::VarPat(_) => unreachable!(),
        }
    }
}

struct Macros<'a> {
    pat_rep_pairs: Vec<(PatternNode<'a>, ReplacementNode<'a>)>,
}

fn extract_pattern<'a>(ast: &Ast<'a>, has_var_pat: bool) -> PatternNode<'a> {
    match ast {
        Ast::Int(_, _) | Ast::Char(_, _) | Ast::Float(_, _) | Ast::Str(_, _) | Ast::Key(_, _) => {
            PatternNode::Literal(ast.clone())
        }

        Ast::Symbol(_, s) => PatternNode::Symbol(*s),
        Ast::SymbolOwned(_, _) => todo!(),
        Ast::Quote(_, v) => PatternNode::Literal((**v).clone()),

        Ast::SExpr(_, v) => {
            if !v.is_empty() && matches!(v[0], Ast::Symbol(_, "..")) && !has_var_pat {
                PatternNode::VarPat(v[1..].iter().map(|v| extract_pattern(v, true)).collect())
            } else {
                PatternNode::SExpr(v.iter().map(|v| extract_pattern(v, has_var_pat)).collect())
            }
        }

        Ast::Attribute(_, v, a) => PatternNode::Attribute(
            Box::new(extract_pattern(&**v, has_var_pat)),
            Box::new(extract_pattern(&**a, has_var_pat)),
        ),
    }
}

fn extract_replacement<'a>(ast: &Ast<'a>, has_var_pat: bool) -> ReplacementNode<'a> {
    match ast {
        Ast::Int(_, _) | Ast::Char(_, _) | Ast::Float(_, _) | Ast::Str(_, _) | Ast::Key(_, _) => {
            ReplacementNode::Literal(ast.clone())
        }

        Ast::Symbol(_, s) => ReplacementNode::Symbol(*s),
        Ast::SymbolOwned(_, _) => todo!(),
        Ast::Quote(_, v) => ReplacementNode::Literal((**v).clone()),

        Ast::SExpr(_, v) => {
            if !v.is_empty() && matches!(v[0], Ast::Symbol(_, "..")) && !has_var_pat {
                ReplacementNode::VarPat(
                    v[1..]
                        .iter()
                        .map(|v| extract_replacement(v, true))
                        .collect(),
                )
            } else {
                ReplacementNode::SExpr(
                    v.iter()
                        .map(|v| extract_replacement(v, has_var_pat))
                        .collect(),
                )
            }
        }

        Ast::Attribute(_, v, a) => ReplacementNode::Attribute(
            Box::new(extract_replacement(&**v, has_var_pat)),
            Box::new(extract_replacement(&**a, has_var_pat)),
        ),
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
                        let pattern = PatternNode::SExpr(
                            v.iter()
                                .skip(1)
                                .map(|v| extract_pattern(v, false))
                                .collect(),
                        );
                        (name, pattern)
                    }
                    _ => return,
                };

                let replacement = ReplacementNode::SExpr(
                    [ReplacementNode::Literal(Ast::Symbol(0..0, "#inline"))]
                        .into_iter()
                        .chain(v[2..].iter().map(|v| extract_replacement(v, false)))
                        .collect(),
                );
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
        Ast::Quote(_, v) => apply_macros(v, macros),

        Ast::SExpr(_, v) if !v.is_empty() && !matches!(v[0], Ast::Symbol(_, "defmacro")) => {
            let mut b = false;
            for v in v.iter_mut() {
                b |= apply_macros(v, macros);
            }

            match v.get(0) {
                Some(Ast::Symbol(_, macro_name)) if macros.contains_key(*macro_name) => {
                    if let Some(macro_) = macros.get(*macro_name) {
                        for (pat, rep) in macro_.pat_rep_pairs.iter().rev() {
                            let mut map = HashMap::new();
                            if pat.matches(&Ast::SExpr(0..0, v[1..].to_vec()), &mut map) {
                                *ast = rep.replace(&map);
                                return true;
                            }
                        }

                        b
                    } else {
                        b
                    }
                }

                _ => b,
            }
        }

        Ast::Attribute(_, v, a) => apply_macros(v, macros) | apply_macros(a, macros),

        _ => false,
    }
}

pub fn execute_macros(asts: &mut Vec<Ast<'_>>) {
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

        let mut includes = Vec::new();
        for (i, ast) in asts.iter_mut().enumerate() {
            perform_post_macro_transformations(ast);
            match ast {
                Ast::SExpr(_, v) if matches!(v.get(0), Some(Ast::Symbol(_, "#inline"))) => {
                    includes.push(i);
                }

                _ => (),
            }
        }

        for i in includes.into_iter().rev() {
            let vals = asts.remove(i);
            match vals {
                Ast::SExpr(_, vals) => {
                    for (j, val) in vals.into_iter().skip(1).enumerate() {
                        asts.insert(i + j, val);
                    }
                }

                _ => unreachable!(),
            }
        }
    }
}

fn perform_post_macro_transformations(ast: &mut Ast<'_>) {
    match ast {
        Ast::Int(_, _)
        | Ast::Char(_, _)
        | Ast::Float(_, _)
        | Ast::Str(_, _)
        | Ast::Symbol(_, _)
        | Ast::SymbolOwned(_, _)
        | Ast::Key(_, _) => (),

        Ast::Quote(_, v) => perform_post_macro_transformations(v),

        Ast::SExpr(s, v) if v.len() == 2 && matches!(v.get(0), Some(Ast::Symbol(_, "#quote"))) => {
            let v = v.swap_remove(1);
            *ast = Ast::Quote(s.clone(), Box::new(v));
        }

        /*
        Ast::SExpr(s, v) if v.len() == 3 && matches!(v.get(0), Some(Ast::Symbol(_, "#concat"))) => {
            match (&v[1], &v[2]) {
                (Ast::Symbol(_, a), Ast::Symbol(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}{}", a, b)),
                (Ast::Symbol(_, a), Ast::Key(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}:{}", a, b)),
                (Ast::Symbol(_, a), Ast::Int(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}{}", a, b)),
                (Ast::SymbolOwned(_, a), Ast::Symbol(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}{}", a, b)),
                (Ast::SymbolOwned(_, a), Ast::Key(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}:{}", a, b)),
                (Ast::SymbolOwned(_, a), Ast::Int(_, b)) => *ast = Ast::SymbolOwned(s.clone(), format!("{}{}", a, b)),
                _ => (),
            }
        }
        */
        Ast::SExpr(_, v) if !matches!(v.get(0), Some(Ast::Symbol(_, "defmacro"))) => {
            let mut includes = Vec::new();
            for (i, v) in v.iter_mut().enumerate() {
                perform_post_macro_transformations(v);
                match v {
                    Ast::SExpr(_, v) if matches!(v.get(0), Some(Ast::Symbol(_, "#inline"))) => {
                        includes.push(i);
                    }

                    _ => (),
                }
            }

            for i in includes.into_iter().rev() {
                let vals = v.remove(i);
                match vals {
                    Ast::SExpr(_, vals) => {
                        for (j, val) in vals.into_iter().skip(1).enumerate() {
                            v.insert(i + j, val);
                        }
                    }

                    _ => unreachable!(),
                }
            }
        }

        Ast::SExpr(_, v) => {
            if let Some(v) = v.get_mut(1) {
                perform_post_macro_transformations(v);
            }
        }

        Ast::Attribute(_, v, a) => {
            perform_post_macro_transformations(v);
            perform_post_macro_transformations(a);
        }
    }
}
