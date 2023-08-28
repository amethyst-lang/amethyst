use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("def main() do let x = 2 helper() x = 2 helper() end");
    parser.op_data = Parser::default_op_data();
    println!("{:?}", parser.parse());
}