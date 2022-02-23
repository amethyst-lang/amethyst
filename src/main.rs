use std::collections::HashMap;

use amethyst::frontend::{macros, correctness};
use amethyst::parser::TopParser;

fn main() {
    let mut asts = TopParser::new()
        .parse(
            r#"
    (defmacro while con stats &key else
        (loop
            (cond
                (con stats)
                (true (break else)))))
    (while true
        (seq
            (println "uwu"))
        :else
            (+ 1 2))
    "#,
        )
        .unwrap();
    let mut map = HashMap::new();
    macros::extract_macros(&mut map, &asts);
    macros::replace_macros(&map, &mut asts);
    let sexprs = correctness::lower(asts).unwrap();

    println!("{:#?}", sexprs);
}
