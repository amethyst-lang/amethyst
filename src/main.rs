use std::collections::HashMap;

use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
    r#"
        (defstruct IntFloat
            (int i32)
            (float f64))

        (defstruct Pair
            (first 'a)
            (second 'b))

        (let int-float =
            (inst IntFloat
                (int 69)
                (float 420.0)))
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
