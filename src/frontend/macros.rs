use std::ops::RangeFrom;

use std::collections::{hash_map::Entry, HashMap};

use super::parsing::Ast;

#[derive(Copy, Clone, PartialEq, Debug)]
enum ArgType {
    Ordered,
    Optional,
    Key,
    Rest,
}

#[derive(PartialEq, Debug)]
struct Arg<'input> {
    name: &'input str,
    type_: ArgType,
    value: Option<Ast<'input>>,
}

#[derive(PartialEq, Debug)]
pub struct Macro<'input> {
    name: &'input str,
    args: Vec<Arg<'input>>,
    body: Option<Ast<'input>>,
}

enum CapturedArg {
    None,
    Single(usize),
    Rest(RangeFrom<usize>),
}

impl<'input> Macro<'input> {
    fn args_bind(&self, ast: &[Ast<'input>]) -> Option<HashMap<&'input str, CapturedArg>> {
        let mut bindings = HashMap::new();

        for arg in self.args.iter() {
            bindings.insert(arg.name, CapturedArg::None);
        }

        let mut ordered_arg = 0;
        let mut last_key = None;
        for (i, arg) in ast.iter().enumerate() {
            if let Ast::Key(_, key) = arg {
                if last_key.is_some() {
                    return None;
                }

                for arg in self.args.iter() {
                    if arg.name == *key {
                        last_key = Some(arg.name);
                    }
                }

                last_key?;
            } else if let Some(key) = last_key {
                bindings.insert(key, CapturedArg::Single(i));
                last_key = None;
            } else {
                let arg = self.args.get(ordered_arg)?;
                if let ArgType::Ordered | ArgType::Optional = arg.type_ {
                    bindings.insert(arg.name, CapturedArg::Single(i));

                    ordered_arg += 1;
                    while let Some(Arg {
                        type_: ArgType::Key,
                        ..
                    }) = self.args.get(ordered_arg)
                    {
                        ordered_arg += 1;
                    }
                } else if let ArgType::Rest = arg.type_ {
                    for arg in ast.iter().skip(i + 1) {
                        if let Ast::Key(_, _) = arg {
                            return None;
                        }
                    }

                    bindings.insert(arg.name, CapturedArg::Rest(i..));
                    return Some(bindings);
                }
            }
        }

        if let Some(Arg {
            type_: ArgType::Ordered | ArgType::Optional,
            ..
        }) = self.args.get(ordered_arg)
        {
            None
        } else {
            Some(bindings)
        }
    }
}

fn replace_macro_args<'input>(
    ast: &mut Ast<'input>,
    map: &HashMap<&'input str, CapturedArg>,
    args: &[Ast<'input>],
) {
    match ast {
        Ast::Int(_, _) | Ast::Char(_, _) | Ast::Float(_, _) | Ast::Str(_, _) | Ast::Key(_, _) => (),

        Ast::Symbol(_, name) => {
            if let Some(cap) = map.get(name) {
                match cap {
                    CapturedArg::None => *ast = Ast::SExpr(ast.span(), vec![]),
                    CapturedArg::Single(single) => *ast = args[*single].clone(),
                    CapturedArg::Rest(rest) => {
                        *ast = {
                            let sexpr = args[rest.clone()].to_vec();
                            Ast::SExpr(
                                sexpr.first().map(|v| v.span().start).unwrap_or(0)
                                    ..sexpr.last().map(|v| v.span().end).unwrap_or(0),
                                sexpr,
                            )
                        }
                    }
                }
            }
        }

        Ast::Quote(_, quote) => replace_macro_args(quote, map, args),
        Ast::Comma(_, comma) => replace_macro_args(comma, map, args),
        Ast::Backtick(_, backtick) => replace_macro_args(backtick, map, args),
        Ast::Splice(_, splice) => replace_macro_args(splice, map, args),

        Ast::SExpr(_, sexpr) => {
            for ast in sexpr {
                replace_macro_args(ast, map, args);
            }
        }

        Ast::Attribute(_, attrs) => {
            for attr in attrs {
                replace_macro_args(attr, map, args);
            }
        }
    }
}

fn replace_macros_helper<'input>(
    map: &HashMap<&'input str, Vec<Macro<'input>>>,
    ast: &mut Ast<'input>,
) -> bool {
    match ast {
        Ast::Int(_, _)
        | Ast::Char(_, _)
        | Ast::Float(_, _)
        | Ast::Str(_, _)
        | Ast::Symbol(_, _)
        | Ast::Key(_, _) => false,

        Ast::Quote(_, quote) => replace_macros_helper(map, quote),
        Ast::Comma(_, comma) => replace_macros_helper(map, comma),
        Ast::Backtick(_, backtick) => replace_macros_helper(map, backtick),
        Ast::Splice(_, splice) => replace_macros_helper(map, splice),

        Ast::SExpr(_, v) => match v.first() {
            Some(Ast::Symbol(_, name)) if map.contains_key(name) => {
                for macro_ in map.get(name).unwrap().iter().rev() {
                    if let Some(bindings) = macro_.args_bind(&v[1..]) {
                        let mut new = macro_
                            .body
                            .clone()
                            .unwrap_or_else(|| Ast::SExpr(0..0, Vec::new()));

                        replace_macro_args(&mut new, &bindings, &v[1..]);

                        *ast = new;
                        return true;
                    }
                }

                false
            }

            _ => {
                let mut updated = false;
                for ast in v {
                    if replace_macros_helper(map, ast) {
                        updated = true;
                    }
                }

                updated
            }
        },

        Ast::Attribute(_, attrs) => {
            let mut updated = false;
            for attr in attrs {
                if replace_macros_helper(map, attr) {
                    updated = true;
                }
            }

            updated
        }
    }
}

pub fn replace_macros<'input>(
    map: &HashMap<&'input str, Vec<Macro<'input>>>,
    asts: &mut [Ast<'input>],
) {
    while {
        let mut updated = false;
        for ast in asts.iter_mut() {
            if replace_macros_helper(map, ast) {
                updated = true;
            }
        }
        updated
    } {}
}

pub fn extract_macros<'input>(
    map: &mut HashMap<&'input str, Vec<Macro<'input>>>,
    asts: &[Ast<'input>],
) {
    for ast in asts {
        if let Ast::SExpr(_, sexpr) = ast {
            if let Some(Ast::Symbol(_, "defmacro")) = sexpr.get(0) {
                if let Some(Ast::Symbol(_, name)) = sexpr.get(1) {
                    let mut args = Vec::new();
                    let mut type_ = ArgType::Ordered;

                    for arg in sexpr[2..sexpr.len() - 1].iter() {
                        match arg {
                            Ast::Symbol(_, "&optional") => type_ = ArgType::Optional,
                            Ast::Symbol(_, "&key") => type_ = ArgType::Key,
                            Ast::Symbol(_, "&rest") => type_ = ArgType::Rest,
                            Ast::Symbol(_, name) => {
                                args.push(Arg {
                                    name,
                                    type_,
                                    value: None,
                                });
                            }

                            Ast::SExpr(_, sexpr) if sexpr.len() == 2 => {
                                if let Ast::Symbol(_, name) = sexpr[0] {
                                    args.push(Arg {
                                        name,
                                        type_,
                                        value: Some(sexpr[1].clone()),
                                    })
                                }
                            }

                            _ => (),
                        }
                    }

                    let body = if sexpr.len() > 2 { sexpr.last() } else { None };

                    match map.entry(*name) {
                        Entry::Occupied(mut v) => {
                            v.get_mut().push(Macro {
                                name: *name,
                                args,
                                body: body.cloned(),
                            });
                        }

                        Entry::Vacant(v) => {
                            v.insert(vec![Macro {
                                name: *name,
                                args,
                                body: body.cloned(),
                            }]);
                        }
                    }
                }
            }
        }
    }
}
