#![feature(let_else)]
#![feature(box_syntax)]
#![feature(box_patterns)]

use chumsky::error::SimpleReason;
use std::{env::args, fs::write};
use std::fs::read_to_string;
use std::io;

use crate::{
    parser::{parser, Node},
    tokenizer::Token,
};
use chumsky::Parser;

use crate::tokenizer::tokenize;

mod compile;
mod parser;
mod tokenizer;

fn main() -> io::Result<()> {
    let file_name = args().nth(1).unwrap();

    let file = read_to_string(file_name)?;

    let tokens = tokenize(file);

    let nodes = parser()
        .parse(tokens.clone())
        .map_err(|e| {
            for err in e {
                println!(
                    "{} at {:?} (near `{}`)",
                    match err.reason() {
                        SimpleReason::Unexpected => {
                            format!("Unexpected node")
                        }
                        SimpleReason::Unclosed { delimiter, .. } => {
                            format!("Unclosed delimiter {delimiter}")
                        }
                        SimpleReason::Custom(reason) => {
                            reason.to_owned()
                        }
                    },
                    err.span(),
                    tokens.get(err.span().start - 1).unwrap_or(&Token::Ident(
                        "Failed to fetch source of error; my best advice is too look near the EOF!"
                            .into()
                    ))
                );
            }
        })
        .unwrap();

    let nodes = nodes.into_iter().map(|e| e.to_string());

    let compiled = nodes.collect::<Vec<String>>().join("\n");

    write("./out.js", compiled).unwrap();

    Ok(())
}
