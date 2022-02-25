use std::collections::HashMap;

use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
            r#"
        (defun add (a i32) (b i32) : i32
            (+ a b))
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    println!("{:#?}", sexprs);
    correctness::check(&mut sexprs).unwrap();

    println!("{:#?}", sexprs);
}
