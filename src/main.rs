use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new("declfix ++ right 90 def ++() do let _ = (++)(3, 2) ++ 3 end");
    parser.op_data = Parser::default_op_data();
    parser.op_data.extend(parser.extract_op_data());
    println!("{:?}", parser.parse());
}