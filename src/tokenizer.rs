use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

use rust_decimal::Decimal;

use crate::parser::Node;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum Keyword {
    Let,
    Const,
    Fun,
    Class,
    Get,
    Set,
    Obj,
    Drop,
    Throw,
    Match,
    Matches,
    New,
    Import,
    Export,
    Include,
    Is,
    Isnt,
    And,
    Or,
    Not,
    If,
    Else,
    Elif,
    For,
    While,
    Return,
    Null,
    Undefined,
}

// JS equivalents of Kong keywords.
impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Keyword::Let => {
                    "let"
                }
                Keyword::Const => {
                    "const"
                }
                Keyword::Fun => {
                    "function"
                }
                Keyword::Class => {
                    "class"
                }
                Keyword::Get => {
                    "get"
                }
                Keyword::Set => {
                    "set"
                }
                Keyword::Obj => {
                    ""
                }
                Keyword::Drop => {
                    "delete"
                }
                Keyword::Throw => {
                    "throw"
                }
                Keyword::Match => {
                    "switch"
                }
                Keyword::Matches => {
                    "=="
                }
                Keyword::New => {
                    "new"
                }
                Keyword::Import => {
                    "import"
                }
                Keyword::Export => {
                    "export"
                }
                Keyword::Include => {
                    "include"
                }
                Keyword::Is => {
                    "==="
                }
                Keyword::Isnt => {
                    "!=="
                }
                Keyword::And => {
                    "&&"
                }
                Keyword::Or => {
                    "||"
                }
                Keyword::Not => {
                    "!"
                }
                Keyword::If => {
                    "if"
                }
                Keyword::Else => {
                    "else"
                }
                Keyword::Elif => {
                    "else if"
                }
                Keyword::For => {
                    "for"
                }
                Keyword::While => {
                    "while"
                }
                Keyword::Return => {
                    "return"
                }
                Keyword::Null => {
                    "null"
                }
                Keyword::Undefined => {
                    "undefined"
                }
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum Math {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Root,
    /// Greater than
    Gt,
    /// Less than
    Lt,
    Xor,
    Range,
}

impl Math {
    pub fn apply(&self, lhs: Node, rhs: Node) -> Node {
        match self {
            Math::Add => Node::Add(box lhs, box rhs),
            Math::Sub => Node::Sub(box lhs, box rhs),
            Math::Mul => Node::Mul(box lhs, box rhs),
            Math::Div => Node::Div(box lhs, box rhs),
            Math::Pow => Node::Pow(box lhs, box rhs),
            Math::Mod => Node::Mod(box lhs, box rhs),
            Math::Root => Node::Root(box lhs, box rhs),
            Math::Gt => Node::Gt(box lhs, box rhs),
            Math::Lt => Node::Lt(box lhs, box rhs),
            Math::Xor => Node::Xor(box lhs, box rhs),
            Math::Range => Node::Range(box lhs, box rhs),
        }
    }
}

impl Display for Math {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Math::Add => {
                    "+"
                }
                Math::Sub => {
                    "-"
                }
                Math::Mul => {
                    "*"
                }
                Math::Div => {
                    "/"
                }
                Math::Pow => {
                    "**"
                }
                Math::Mod => {
                    "%"
                }
                Math::Root => {
                    panic!("Impossible for Sqrt!")
                }
                Math::Gt => {
                    ">"
                }
                Math::Lt => {
                    "<"
                }
                Math::Xor => {
                    "^"
                }
                Math::Range => {
                    panic!("Impossible for range!")
                }
            }
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(kw) => Display::fmt(kw, f),
            Token::Math(m) => Display::fmt(m, f),
            Token::Equals => {
                write!(f, "=")
            }
            Token::DotAccessor => {
                write!(f, ".")
            }
            Token::AnonymousArrow => {
                write!(f, "->")
            }
            Token::Colon => {
                write!(f, ":")
            }
            Token::MacroInvocation => {
                write!(f, "!")
            }
            Token::MacroVariable => {
                write!(f, "$")
            }
            Token::Ident(id) => Display::fmt(id, f),
            Token::Str(str) => Debug::fmt(str, f),
            Token::Bool(b) => Display::fmt(b, f),
            Token::Num(n) => Display::fmt(n, f),
            Token::Group(g) => {
                write!(
                    f,
                    "{}",
                    g.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                )
            }
            Token::Array(g) => {
                write!(
                    f,
                    "{}",
                    g.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(","),
                )
            }
            Token::Block(g) => {
                write!(
                    f,
                    "{}",
                    g.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(";"),
                )
            }
        }
    }
}

// Enums do not support repr([char; 2]) so this'll do.
#[allow(non_snake_case)]
pub mod BlockType {
    #[allow(non_upper_case_globals)]
    pub const Array: [char; 2] = ['[', ']'];
    #[allow(non_upper_case_globals)]
    pub const Block: [char; 2] = ['{', '}'];
    #[allow(non_upper_case_globals)]
    pub const Group: [char; 2] = ['(', ')'];
}

// All possible tokens
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
    Keyword(Keyword),
    Math(Math),
    Equals,
    DotAccessor,
    AnonymousArrow,
    Colon,
    MacroInvocation,
    MacroVariable,
    Ident(String),
    Str(String),
    Bool(bool),
    Num(Decimal),
    Group(Vec<Token>),
    Array(Vec<Token>),
    Block(Vec<Token>),
}

pub fn tokenize(file: String) -> Vec<Token> {
    // File turned into an iterator for ease of use later on
    let mut file = file.chars().peekable();

    // What is returned at the end
    let mut final_out = vec![];

    'a: while let Some(char) = file.next() {
        match char {
            // Comments, or division
            '/' => match file.peek() {
                Some('*') => {
                    while Some('*') != file.next()
                        && Some('/') != file.next()
                        && None != file.next()
                    {}
                }
                Some('/') => while Some('\n') != file.next() && None != file.next() {},
                Some(_) => final_out.push(Token::Math(Math::Div)),
                None => panic!("Unexpected EOF"),
            },
            // Idents
            'a'..='z' | 'A'..='Z' | '_' | '#' => {
                let mut ident = String::from(char);

                while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = file.peek() {
                    ident.push(file.next().unwrap());
                }

                // Matches potential keywords.
                let out = match ident.as_str() {
                    "let" => Token::Keyword(Keyword::Let),
                    "const" => Token::Keyword(Keyword::Const),
                    "class" => Token::Keyword(Keyword::Class),
                    "drop" => Token::Keyword(Keyword::Drop),
                    "fun" => Token::Keyword(Keyword::Fun),
                    "get" => Token::Keyword(Keyword::Get),
                    "set" => Token::Keyword(Keyword::Set),
                    "obj" => Token::Keyword(Keyword::Obj),
                    "throw" => Token::Keyword(Keyword::Throw),
                    "match" => Token::Keyword(Keyword::Match),
                    "matches" => Token::Keyword(Keyword::Matches),
                    "new" => Token::Keyword(Keyword::New),
                    "import" => Token::Keyword(Keyword::Import),
                    "export" => Token::Keyword(Keyword::Export),
                    "include" => Token::Keyword(Keyword::Include),
                    "is" => Token::Keyword(Keyword::Is),
                    "isnt" => Token::Keyword(Keyword::Isnt),
                    "not" => Token::Keyword(Keyword::Not),
                    "and" => Token::Keyword(Keyword::And),
                    "or" => Token::Keyword(Keyword::Or),
                    "if" => Token::Keyword(Keyword::If),
                    "else" => Token::Keyword(Keyword::Else),
                    "elif" => Token::Keyword(Keyword::Elif),
                    "for" => Token::Keyword(Keyword::For),
                    "while" => Token::Keyword(Keyword::While),
                    "return" => Token::Keyword(Keyword::Return),
                    "null" => Token::Keyword(Keyword::Null),
                    "undefined" => Token::Keyword(Keyword::Undefined),
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    _ => Token::Ident(ident),
                };

                final_out.push(out);
            }
            // Strings
            chr @ '"' | chr @ '\'' => {
                let mut string = String::new();

                while let Some(char) = file.next() {
                    if char == chr {
                        break;
                    }
                    if char == '\\' {
                        match file.next().unwrap() {
                            'n' => string.push('\n'),
                            'r' => string.push('\r'),
                            't' => string.push('\t'),
                            '0' => string.push('\0'),
                            'u' => {
                                let hexcode = String::from_iter([
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                ]);

                                let num = u8::from_str_radix(&hexcode, 16).unwrap();

                                string.push(num as char);
                            }
                            'x' => {
                                let hexcode = String::from_iter([
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                    file.next().unwrap(),
                                ]);

                                let num = u8::from_str_radix(&hexcode, 16).unwrap();

                                string.push(num as char);
                            }
                            a => string.push(a),
                        }
                        continue;
                    }

                    string.push(char);
                }

                final_out.push(Token::Str(string))
            }
            // Anonymous arrow or subtraction
            '-' => {
                if Some(&'>') == file.peek() {
                    file.next();
                    final_out.push(Token::AnonymousArrow);
                } else {
                    final_out.push(Token::Math(Math::Sub));
                }
            }
            // Addition
            '+' => final_out.push(Token::Math(Math::Add)),
            // Multiplication or exponent
            '*' => {
                if Some(&'*') == file.peek() {
                    final_out.push(Token::Math(Math::Pow));
                    file.next();
                } else {
                    final_out.push(Token::Math(Math::Mul))
                }
            }
            // Modulo
            '%' => final_out.push(Token::Math(Math::Mod)),
            // Xor
            '^' => final_out.push(Token::Math(Math::Xor)),
            // Gt
            '>' => final_out.push(Token::Math(Math::Gt)),
            // Lt
            '<' => final_out.push(Token::Math(Math::Lt)),
            // Root
            '|' => final_out.push(Token::Math(Math::Root)),
            // Equals
            '=' => final_out.push(Token::Equals),
            // HACK: The implementation to check if a dot is part of a range
            // or a number is hacky.
            // Numbers
            '0'..='9' => {
                let mut num = String::from(char);

                while let Some(chr @ '0'..='9' | chr @ '.') = file.peek() {
                    if *chr == '.' {
                        let dot = file.next().unwrap();
                        match file.peek().unwrap() {
                            '.' => {
                                final_out.push(Token::Num(Decimal::from_str(&num).unwrap()));
                                final_out.push(Token::Math(Math::Range));
                                file.next();
                                continue 'a;
                            }
                            '0'..='9' => {
                                num.push(dot);
                            }
                            _ => {
                                final_out.push(Token::Num(Decimal::from_str(&num).unwrap()));
                                final_out.push(Token::DotAccessor);
                                continue 'a;
                            }
                        }
                    }
                    num.push(file.next().unwrap());
                }

                final_out.push(Token::Num(Decimal::from_str(&num).unwrap()));
            }
            // "this.", compacted to @
            '@' => {
                final_out.push(Token::Ident("this".into()));
                final_out.push(Token::DotAccessor);
            }
            // Ranges or field accessors
            '.' => {
                if let Some('.') = file.peek() {
                    file.next();
                    final_out.push(Token::Math(Math::Range))
                } else {
                    final_out.push(Token::DotAccessor);
                }
            }
            // Colon (for indexing)
            ':' => final_out.push(Token::Colon),
            // Macro invocation
            '!' => final_out.push(Token::MacroInvocation),
            // Macro variables
            '&' => final_out.push(Token::MacroVariable),
            // Scopes/blocks, groups, and arrays
            bracket @ '[' | bracket @ '(' | bracket @ '{' => {
                let brackets = match bracket {
                    '[' => BlockType::Array,
                    '(' => BlockType::Group,
                    '{' => BlockType::Block,
                    _ => unreachable!(),
                };

                let mut src = String::new();

                let mut bracket_count: usize = 1;

                let mut in_string = false;

                while bracket_count != 0 {
                    let char = file.next().unwrap();

                    if char == '"' || char == '\'' {
                        in_string = !in_string;
                    }

                    if char == '\\' && in_string {
                        src.push(char);
                        src.push(file.next().unwrap());
                    }

                    if char == brackets[1] {
                        if !in_string {
                            bracket_count -= 1;
                        }

                        if bracket_count != 0 {
                            src.push(char);
                        }

                        continue;
                    } else if char == brackets[0] {
                        if !in_string {
                            bracket_count += 1;
                        }

                        src.push(char);

                        continue;
                    }

                    src.push(char);
                }

                final_out.push(match brackets {
                    BlockType::Array => Token::Array(tokenize(src)),
                    BlockType::Block => Token::Block(tokenize(src)),
                    BlockType::Group => Token::Group(tokenize(src)),
                    [_, _] => unreachable!(),
                });
            }
            // Ignore whitespace
            chr if chr.is_whitespace() => {}
            // Panic for unexpected characters
            _ => panic!("Unexpected char {char}! {final_out:?}"),
        }
    }

    final_out
}
