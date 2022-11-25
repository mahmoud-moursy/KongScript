#![feature(box_syntax, box_patterns)]

// use chumsky::error::SimpleReason;
use std::fs::read_to_string;
use std::{env::args, fs::write};

use parser::parse;

use crate::{errors::Error, tokenizer::tokenize, tokenizer::Token};

mod errors;
mod parser;
mod tokenizer;

fn main() -> Result<(), Error> {
    let file_name = args().nth(1).unwrap_or("./main.kong".into());

    let file = read_to_string(file_name)?;

    let tokens = tokenize(file)?;

    #[cfg(debug_assertions)]
    println!("{tokens:?}");

    let nodes = parse(tokens);

    let nodes: Vec<String> = nodes.into_iter().map(|e| e.compile()).collect();

    #[cfg(debug_assertions)]
    println!("{nodes:?}");

    let compiled = nodes.join("\n");

    write("./out.js", compiled).unwrap();

    Ok(())
}
