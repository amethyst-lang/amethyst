use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("if true then 2 else 3");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{:?}", ast);
}
