use std::collections::HashMap;

use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
            r#"
        (defun add (a i32) (b i32) : i32
            5)
        (defun add (a f32) (b f32) : f32
            2.3)
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    let mut func_map = HashMap::new();
    correctness::extract_signatures(&sexprs, &mut func_map);
    println!("{:#?}", sexprs);
    println!("{:#?}", func_map);
    correctness::check(&mut sexprs).unwrap();

    println!("{:#?}", sexprs);
}
