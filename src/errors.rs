use std::{fmt::Display, io, num::ParseIntError};

use thiserror::{Error};
use crate::{Token, parser::Node};

#[derive(Debug)]
pub enum Type {
    Node(Node),
    Token(Token),
    Char(char),
    Str(String)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Node(node) => write!(f, "{node:?}"),
            Type::Token(token) => write!(f, "{token}"),
            Type::Char(character) => write!(f, "{character}"),
            Type::Str(string) => write!(f, "{string}"),
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected token `{0}")]
    Unexpected(Type),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Internal file reading error: {io:?}")]
    Io {
        #[from]
        io: io::Error
    },
    #[error("Error parsing integer {parse_int}")]
    ParserIntErr {
        #[from]
        parse_int: ParseIntError
    },
    #[error("Error parsing decimal {parse_decimal}")]
    DecimalErr {
        #[from]
        parse_decimal: rust_decimal::Error
    }
}