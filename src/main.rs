use amethyst::lexer::Lexer;
use amethyst::{parse, typecheck};

fn main() {
    let mut lexer = Lexer::new("let even n = if n == 0 then true else odd (n - 1)\nlet odd n = if n == 0 then false else even (n - 1)");
    let mut asts = parse::parse(&mut lexer).expect("should work");
    for ast in asts.iter() {
        println!("{}", ast);
    }

    println!();

    typecheck::typecheck(&mut asts).expect("should work");

    for ast in asts {
        println!("{}", ast);
    }
}
