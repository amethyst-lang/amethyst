use clap::Parser as ArgParser;

use amethyst::{parse::Parser, typecheck};

#[derive(ArgParser)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let contents = match std::fs::read_to_string(args.filename) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error reading file: {}", e);
            std::process::exit(1);
        }
    };

    let mut parser = Parser::new(&contents);
    parser.op_data = Parser::default_op_data();
    parser.op_data.extend(parser.extract_op_data());
    let ast = match parser.parse() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error parsing: {:?}", e);
            std::process::exit(1);
        }
    };

    match typecheck::typecheck(&ast) {
        Ok(_) => println!("successfully typechecked!"),
        Err(e) => {
            eprintln!("error typechecking: {}", e.message);
            std::process::exit(1);
        }
    }

    println!("{:?}", ast);
}
