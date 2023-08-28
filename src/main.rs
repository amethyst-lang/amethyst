use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("def main() do if true then helper() else other() end end");
    parser.op_data = Parser::default_op_data();
    println!("{:?}", parser.parse());
}