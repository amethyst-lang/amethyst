use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("forall (x: type) let id (x: x) -> x = x");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{}", ast);
}
