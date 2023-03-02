use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("f x y z");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{:?}", ast);
}
