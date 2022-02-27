use std::collections::HashMap;

use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
    r#"
    /*
        (defun add (a i69) (b i42) : i32 3)
        (defun add (a f32) (b f32) : f32 3.0)
        (defun add (a f32) (b f32) : i32 3)
        (add 3 4)
        */
        (defun take (a 'a) : i32 2)
        (take 2)
        (take (: i3 3))
        (take 4.5)
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    let mut func_map = HashMap::new();
    correctness::extract_signatures(&sexprs, &mut func_map);
    correctness::check(&mut sexprs, &func_map).unwrap();
    println!("{:#?}", sexprs);
}
