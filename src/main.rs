use amethyst::parse::Parser;

fn main() {
    let mut parser = Parser::new(
        "
        type Option[a] as
            Some(a)
            None
        end

        type List[a] as
            ::(a, List[a])
            Nil
        end

        type Bool as
            True
            False
        end

        type Gadt[a] as
            I(Int) -> Gadt[Int]
            B(Bool) -> Gadt[Bool]
        end
        ");
    parser.op_data = Parser::default_op_data();
    parser.op_data.extend(parser.extract_op_data());
    println!("{:?}", parser.parse());
}