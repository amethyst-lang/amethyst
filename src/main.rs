use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("2 * (3 + 4)");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{:?}", ast);
}
