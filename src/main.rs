use std::collections::HashMap;

use amethyst::backend::arch::rv64::RvSelector;
use amethyst::backend::sexpr_lowering;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let contents = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    /*
    let output = std::env::args().nth(2).unwrap_or_else(|| String::from("a.out"));
    let object = output + ".o";
    */

    let mut asts = TopParser::new().parse(&contents).unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let mut sexprs = ast_lowering::lower(asts).unwrap();
    let mut func_map = correctness::create_default_signatures();
    correctness::extract_signatures(&sexprs, &mut func_map);
    let mut struct_map = HashMap::new();
    correctness::extract_structs(&sexprs, &mut struct_map);
    correctness::check(&mut sexprs, &func_map, &struct_map).unwrap();

    let ir = sexpr_lowering::lower(sexprs);
    println!("{}", ir);

    let vcode = ir.lower_to_vcode::<_, RvSelector>();
    println!("{}", vcode);
}
