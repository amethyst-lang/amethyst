use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("~ !lmao? .*");
    parser.op_data = Parser::default_op_data();
    println!("{:?}", parser.parse());
}