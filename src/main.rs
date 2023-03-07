use amethyst::lexer::Lexer;
use amethyst::{parse, typecheck};

fn main() {
    let mut lexer = Lexer::new("let apply f (x: i32) -> i32 = f x");
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
