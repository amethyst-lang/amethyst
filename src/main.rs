use std::collections::HashMap;
use std::process::Command;

use amethyst::backend::Generator;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

fn main() {
    let contents = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let mut asts = TopParser::new()
        .parse(&contents).unwrap();
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
    std::fs::write("main.o", gen.emit_object()).unwrap();
    Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg("-o")
        .arg("_start.o")
        .arg("_start.s")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    Command::new("ld")
        .arg("_start.o")
        .arg("main.o")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
