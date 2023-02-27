use amethyst::lexer::Lexer;

fn main() {
    let mut lexer = Lexer::new("13 + 2 * 3 / 4 - 56");
    while let Some(token) = lexer.lex() {
        println!("{:?}", token);
    }
}
