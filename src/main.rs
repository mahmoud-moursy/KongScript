#![feature(let_else)]
#![feature(box_syntax)]
#![feature(box_patterns)]

use chumsky::error::SimpleReason;
use std::{env::args, fs::write};
use std::fs::read_to_string;
use std::io;

use crate::{
    parser::{parse, Node},
    tokenizer::Token,
};
use chumsky::Parser;

use crate::tokenizer::tokenize;

mod parser;
mod tokenizer;

fn main() -> io::Result<()> {
    let file_name = args().nth(1).unwrap();

    let file = read_to_string(file_name)?;

    let tokens = tokenize(file);

    let nodes = parse(tokens.clone());

    let nodes = nodes.into_iter().map(|e| e.to_string());

    let compiled = nodes.collect::<Vec<String>>().join("\n");

    write("./out.js", compiled).unwrap();

    Ok(())
}
