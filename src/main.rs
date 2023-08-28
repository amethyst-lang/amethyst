use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("def <[a](x: a, y: a) -> bool do end def return[m, a](v: a) -> m[a] do end");
    parser.op_data = Parser::default_op_data();
    parser.op_data.extend(parser.extract_op_data());
    println!("{:?}", parser.parse());
}