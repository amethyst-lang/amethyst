use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("1 :: 2 :: nil");
    parser.op_data = Parser::default_op_data();
    println!("{:?}", parser.parse());
}