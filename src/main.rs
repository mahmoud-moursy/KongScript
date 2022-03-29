#![feature(let_else)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(in_band_lifetimes)]

use std::env::args;
use std::fs::{read_to_string, write};
use std::io;

use chumsky::Parser;

use crate::parser::parser;
use crate::tokenizer::tokenize;

mod tokenizer;
mod parser;

fn main() -> io::Result<()> {
    let file_name = args().nth(1).unwrap();

    let file = read_to_string(file_name)?;

    let tokens = tokenize(file);

    let nodes = parser().parse(tokens).unwrap();

    println!("{nodes:?}");

    let out = nodes.into_iter().map(|e| e.to_string()).collect::<Vec<String>>().join("");

    write("./out.js", out).unwrap();

    Ok(())
}
