use amethyst::lexer::Lexer;
use amethyst::{parse, typecheck};

fn main() {
    let mut lexer = Lexer::new(
        "
        let not x = if x then false else true

        forall (a: type)
        type Option
            = Some a
            | None

        type NoEq = Uwu | Owo

        forall (a: type) (b: type)
        type Pair = Pair a b

        class Eq e =
            let eq: e -> e -> bool
        end

        instance Eq i32 =
            let eq a b = a == b
        end

        instance Eq bool =
            let eq a b = if a then b else not b
        end

        forall (a: type) where Eq a
        instance Eq (Option a) =
            let eq a b =
                match Pair a b with
                | Pair (Some x) (Some y) to eq x y
                | Pair None None to true
                | _ to false
                end
        end

        let ne a b = not (eq a b)

        let a = eq 2 3
        let b = eq true false
        let c = eq (Some 2) None
    ",
    );

    let mut asts = parse::parse(&mut lexer).expect("should work");
    for ast in asts.iter() {
        println!("{}", ast);
    }

    println!();

    typecheck::typecheck(&mut asts).expect("should work");

    for ast in asts {
        println!("{}", ast);
    }
}
