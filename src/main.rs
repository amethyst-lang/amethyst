use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("let x = 2 in let y = 3 in x + y");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{:?}", ast);
}
