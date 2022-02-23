use std::collections::HashMap;

use amethyst::frontend::{macros, ast_lowering, correctness};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
    r#"
        (seq
            0
            3.4
            "uwu")
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    correctness::check(&mut sexprs).unwrap();

    println!("{:#?}", sexprs);
}
