use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice};

use amethyst::lexer::Lexer;
use amethyst::parse::ParseError;
use amethyst::{parse, typecheck};

fn main() {
    let filename = "test.amy";
    let contents = 
        "
        forall a
        type Option = Some a | None

        let f x =
            let (Some x) = x in
                x
            else 0
        ";

    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, contents);

    let mut lexer = Lexer::new(contents);

    let mut asts = match parse::parse(&mut lexer) {
        Ok(v) => v,
        Err(ParseError::Error {
            message,
            primary_label,
            primary_label_loc,
            secondary_labels,
            notes,
        }) => {
            let diagnostic = Diagnostic::error()
                .with_message(message)
                .with_labels(vec![
                    Label::primary(file_id, primary_label_loc).with_message(primary_label)
                ].into_iter().chain(secondary_labels.into_iter().map(|(msg, span)| Label::secondary(file_id, span).with_message(msg))).collect())
                .with_notes(notes);

                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = term::Config::default();

                term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("o no");
                std::process::exit(1);
        }

        _ => unreachable!(),
    };

    for ast in asts.iter() {
        println!("{}", ast);
    }

    println!();

    typecheck::typecheck(&mut asts).expect("should work");

    for ast in asts {
        println!("{}", ast);
    }
}
