#![feature(let_else)]
#![feature(box_syntax)]
#![feature(box_patterns)]

use chumsky::error::SimpleReason;
use std::env::args;
use std::fs::read_to_string;
use std::io;

use crate::parser::{parser, Node};
use chumsky::Parser;

use crate::tokenizer::tokenize;

mod compile;
mod parser;
mod tokenizer;

fn main() -> io::Result<()> {
    let file_name = args().nth(1).unwrap();

    let file = read_to_string(file_name)?;

    let tokens = tokenize(file);

    println!("{tokens:?}");

    let mut nodes = parser()
        .parse(tokens.clone())
        .map_err(|e| {
            for err in e {
                println!(
                    "{} at {:?} ({})",
                    match err.reason() {
                        SimpleReason::Unexpected => {
                            "Unexpected token".to_string()
                        }
                        SimpleReason::Unclosed { delimiter, .. } => {
                            format!("Unclosed delimiter {delimiter}")
                        }
                        SimpleReason::Custom(reason) => {
                            reason.to_owned()
                        }
                    },
                    err.span(),
                    tokens[err.span().start - 1]
                );
            }
        })
        .unwrap();

    println!("{}", nodes.remove(0));

    Ok(())
}
