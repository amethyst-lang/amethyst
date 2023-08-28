use amethyst::lexer::Lexer;

fn main() {
    let mut lexer = Lexer::new("test <> 123 () x3\nstuff # lmao\nwow");
    for _ in 0..10 {
        println!("token: {:?}", lexer.lex());
    }
}