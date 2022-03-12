use std::collections::HashMap;
use std::process::Command;

use amethyst::backend::Generator;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let contents = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let output = std::env::args().nth(2).unwrap_or_else(|| String::from("a.out"));
    let object = output.clone() + ".o";

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
    let mut gen = Generator::default();
    gen.compile(sexprs, &struct_map);
    std::fs::write(&object, gen.emit_object()).unwrap();
    let code = Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg("-o")
        .arg("_start.o")
        .arg("_start.s")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    assert_eq!(code.code(), 0);
    let code = Command::new("ld")
        .arg("_start.o")
        .arg(&object)
        .arg("-o")
        .arg(&output)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    assert_eq!(code.code(), 0);
}
