use amethyst::lexer::Lexer;
use amethyst::parse;

fn main() {
    let mut lexer = Lexer::new("let fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)");
    let ast = parse::parse(&mut lexer).expect("should work");
    println!("{}", ast);
}
