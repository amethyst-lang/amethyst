use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("let id x = x");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{}", ast);
}
