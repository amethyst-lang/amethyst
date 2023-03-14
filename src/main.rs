use amethyst::lexer::Lexer;
use amethyst::{parse, typecheck};

fn main() {
    // let mut lexer = Lexer::new("forall (a: type) (b: type) let apply (f: a -> b) (x: a) -> b = f x\nforall (a: type) let id (x: a) -> a = x\nlet a = apply id 2\nlet b = apply id true");
    let mut lexer = Lexer::new("forall (a: type) (b: type) type Pair = Pair a b\nforall (a: type) type Option = Some a | None\nlet f x = match x with | Some x to x + 1 | None to 0 end");
    // let mut lexer = Lexer::new("let apply f x = f x\nlet id x = x\nlet a = apply id 2\nlet b = apply id true");
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
