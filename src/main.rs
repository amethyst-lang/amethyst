use std::collections::HashMap;
use std::path::Path;
use std::fs::File;

use clap::Parser;
use codegem::arch::rv64::RvSelector;
use codegem::arch::urcl::UrclSelector;
use codegem::arch::x64::X64Selector;
use codegem::regalloc::RegAlloc;

use amethyst::backend::sexpr_lowering;
use amethyst::frontend::{ast_lowering, correctness, macros};
use amethyst::parser::TopParser;

#[derive(Parser, Debug)]
struct Args {
    #[arg(value_name = "Input files")]
    input_files: Vec<String>,

    #[arg(short, long, default_value_t = String::from("x64"), value_name = "Target arch")]
    target: String,
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
    let ir = sexpr_lowering::lower(modules.remove(0).1.sexprs);

    match args.target.as_str() {
        "x64" => {
            let mut vcode = ir.lower_to_vcode::<_, X64Selector>();
            vcode.allocate_regs::<RegAlloc>();
            let mut file = File::create(format!("{}.s", vcode.name)).expect("o no");
            vcode.emit_assembly(&mut file).unwrap();
        }

        "rv64" => {
            let mut vcode = ir.lower_to_vcode::<_, RvSelector>();
            vcode.allocate_regs::<RegAlloc>();
            let mut file = File::create(format!("{}.s", vcode.name)).expect("o no");
            vcode.emit_assembly(&mut file).unwrap();
        }

        "urcl" => {
            let mut vcode = ir.lower_to_vcode::<_, UrclSelector>();
            vcode.allocate_regs::<RegAlloc>();
            let mut file = File::create(format!("{}.urcl", vcode.name)).expect("o no");
            vcode.emit_assembly(&mut file).unwrap();
        }

        target => {
            eprintln!("Invalid target `{}`", target);
            std::process::exit(1);
        }
    }
}
