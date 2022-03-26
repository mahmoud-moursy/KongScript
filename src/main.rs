use std::env::args;
use std::fs::read_to_string;
use std::io;

use crate::tokenizer::tokenize;

mod tokenizer;
mod parser;

fn main() -> io::Result<()> {
    let file_name = args().nth(1).unwrap();
    let file = read_to_string(file_name)?;

    let tokens = tokenize(file);

    println!("{tokens:?}");

    Ok(())
}
