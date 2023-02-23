use std::collections::HashMap;
use std::path::Path;
//use std::fs::File;

use amethyst::backend::sexpr_lowering::Compiler;
use clap::Parser;

//use amethyst::backend::sexpr_lowering;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;

#[derive(Parser, Debug)]
struct Args {
    #[arg(value_name = "Input files")]
    input_files: Vec<String>,

    #[arg(short, long, value_name = "Target arch")]
    target: Option<String>,
}

fn main() {
    let args = Args::parse();
    let mut modules = Vec::new();
    let mut exports = HashMap::new();
    for file in args.input_files.iter() {
        let path = Path::new(file).canonicalize().unwrap();
        let contents = std::fs::read_to_string(&path).unwrap();
        let mut asts = TopParser::new().parse(&contents).unwrap();
        macros::execute_macros(&mut asts);
        let sexprs = ast_lowering::lower(asts).unwrap();
        let (module, exported) = correctness::create_module(sexprs);
        modules.push((path.clone(), module));
        exports.insert(path, exported);
    }

    for (name, module) in modules.iter_mut() {
        correctness::check(name, module, &exports).unwrap();
    }

    // TODO: modules
    let context = Context::create();
    let compiler = Compiler::lower(&context, modules.remove(0).1.sexprs);
    println!("{}", compiler.module.to_string());

    let triple = args
        .target
        .map(|v| TargetTriple::create(&v))
        .unwrap_or_else(TargetMachine::get_default_triple);
    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(&triple).expect("must be valid");
    let machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("oh no");
    //compiler.module.set_data_layout(); // TODO: figure this out
    compiler.module.set_triple(&triple);
    machine
        .write_to_file(&compiler.module, FileType::Object, Path::new("a.o"))
        .expect("must work");
}
