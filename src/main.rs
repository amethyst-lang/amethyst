use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("def main() do helper() end def helper() do helper(2 * 3) end");
    parser.op_data = Parser::default_op_data();
    println!("{:?}", parser.parse());
}