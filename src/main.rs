use std::collections::HashMap;

use amethyst::backend::Generator;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
    r#"
        (defun main (a i32) (b i32) : i32
            69)
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    let mut func_map = correctness::create_default_signatures();
    correctness::extract_signatures(&sexprs, &mut func_map);
    let mut struct_map = HashMap::new();
    correctness::extract_structs(&sexprs, &mut struct_map);
    correctness::check(&mut sexprs, &func_map, &struct_map).unwrap();
    println!("{:#?}", sexprs);
    let mut gen = Generator::default();
    gen.compile(sexprs);
    std::fs::write("a.out", gen.emit_object()).unwrap();
}
