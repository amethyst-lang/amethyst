use clap::Parser;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use amethyst::lexer::Lexer;
use amethyst::parse::ParseError;
use amethyst::{parse, typecheck};

#[derive(Parser)]
struct Cli {
    filename: String,
}

fn main() {
    let args = Cli::parse();
    let contents = match std::fs::read_to_string(&args.filename) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error opening file: {}", e);
            std::process::exit(1);
        }
    };

    let mut files = SimpleFiles::new();
    let file_id = files.add(&args.filename, &contents);

    let mut lexer = Lexer::new(&contents);

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
                .with_labels(
                    vec![Label::primary(file_id, primary_label_loc).with_message(primary_label)]
                        .into_iter()
                        .chain(
                            secondary_labels.into_iter().map(|(msg, span)| {
                                Label::secondary(file_id, span).with_message(msg)
                            }),
                        )
                        .collect(),
                )
                .with_notes(notes);

            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = term::Config::default();

            term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("o no");
            std::process::exit(1);
        }

        _ => unreachable!(),
    };

    match typecheck::typecheck(&mut asts) {
        Ok(top_vars) => {
            for (var, type_) in top_vars.iter() {
                println!("{}: {}", var, type_);
            }
        }

        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = term::Config::default();

            for error in errors {
                let diagnostic = Diagnostic::error()
                    .with_message(error.message)
                    .with_labels(
                        vec![Label::primary(file_id, error.primary_label_loc).with_message(error.primary_label)]
                            .into_iter()
                            .chain(
                                error.secondary_labels.into_iter().map(|(msg, span)| {
                                    Label::secondary(file_id, span).with_message(msg)
                                }),
                            )
                            .collect(),
                    )
                    .with_notes(error.notes);
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("o no");
            }

            std::process::exit(1);
        }
    }
}
