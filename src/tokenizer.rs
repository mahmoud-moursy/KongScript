use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::str::{FromStr, Chars};

use rust_decimal::Decimal;

use crate::errors::{Error, Type};
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
    // ===
    Eq,
    // !==
    Ineq,
    // ==
    Is,
    // &&
    And,
    // ||
    Or,
    // !
    Not,
    If,
    Else,
    Elif,
    For,
    While,
    Return,
    Null,
    Undefined,
    Raw
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Keyword::Raw => {
                    "raw#"
                }
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
                    "obj"
                }
                Keyword::Drop => {
                    "drop"
                }
                Keyword::Throw => {
                    "throw"
                }
                Keyword::Match => {
                    "match"
                }
                Keyword::Matches => {
                    "matches"
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
                Keyword::Eq => {
                    "eq"
                }
                Keyword::Ineq => {
                    "ineq"
                }
                Keyword::And => {
                    "and"
                }
                Keyword::Or => {
                    "or"
                }
                Keyword::Not => {
                    "not"
                }
                Keyword::If => {
                    "if"
                }
                Keyword::Else => {
                    "else"
                }
                Keyword::Elif => {
                    "elif"
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
    BitOr,
    BitAnd,
    BitFlip,
    LBitShift,
    RBitShift,
    RZeroFillShift,
}

impl Math {
    pub fn apply(self, lhs: Node, rhs: Node) -> Node {
        Node::Op(self, box lhs, box rhs)
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
                    "\\"
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
                Math::BitOr => {
                    "|"
                }
                Math::BitAnd => {
                    "&"
                }
                Math::BitFlip => {
                    "~"
                }
                Math::LBitShift => {
                    "<<"
                }
                Math::RBitShift => {
                    ">>"
                }
                Math::RZeroFillShift => {
                    ">>>"
                }
                Math::Range => {
                    ".."
                }
            }
        )
    }
}

impl Token {
    pub fn compile(&self) -> String {
        match self {
            Token::Keyword(kw) => kw.to_string(),
            Token::Math(m) => m.to_string(),
            Token::Equals => {
                "=".into()
            }
            Token::Dot => {
                ".".into()
            }
            Token::AnonymousArrow => {
                "->".into()
            }
            Token::Colon => {
                ":".into()
            }
            Token::MacroInvocation => {
                "!".into()
            }
            Token::MacroVariable => {
                "$".into()
            }
            Token::Ident(id) => id.into(),
            Token::Str(str) => str.into(),
            Token::Bool(b) => b.to_string(),
            Token::Num(n) => n.to_string(),
            Token::Group(g) | Token::Array(g) | Token::Block(g) => {
                    g.iter()
                        .map(|e| e.compile())
                        .collect::<Vec<String>>()
                        .join(",")
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
    Dot,
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Math(operator) => write!(f, "{operator}"),
            Token::Equals => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::AnonymousArrow => write!(f, "->"),
            Token::Colon => write!(f, ":"),
            Token::MacroInvocation => write!(f, "!"),
            Token::MacroVariable => write!(f, "@"),
            Token::Ident(ident) => write!(f, "{ident}"),
            // Debug display of String encloses it in quotes and escapes other quotes.
            Token::Str(string) => write!(f, "{string:?}"),
            Token::Bool(boolean) => write!(f, "{boolean}"),
            Token::Num(num) => write!(f, "{num}"),
            Token::Group(group) => write!(f, "( {} )", group.into_iter().map(|x| x.to_string()).collect::<Vec<String>>().join(",")),
            Token::Array(array) => write!(f, "[ {} ]", array.into_iter().map(|x| x.to_string()).collect::<Vec<String>>().join(",")),
            Token::Block(block) => write!(f, "{{\n\t{}\n}}", block.into_iter().map(|x| x.to_string()).collect::<Vec<String>>().join("\n\t")),
        }
    }
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
}

impl Iterator for Tokenizer<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next()
    }
}

impl<'a> Tokenizer<'a> {
    fn next_if(&mut self, func: impl FnOnce(&char) -> bool) -> Option<char> {
        self.input.next_if(func)
    }

    fn next_eq(&mut self, chr: char) -> bool {
        self.input.next_if_eq(&chr).is_some()
    }

    fn next_or(&mut self) -> Result<char, Error> {
        match self.input.peek() {
            Some(_) => Ok(self.next().unwrap()),
            None => Err(Error::UnexpectedEOF)
        }
    }

    fn peek_or(&mut self) -> Result<&char, Error> {
        match self.input.peek() {
            Some(chr_ref) => Ok(chr_ref),
            None => Err(Error::UnexpectedEOF)
        }
    }

    fn peek(&mut self) -> Option<&char> {
        return self.input.peek()
    }

    fn eof(&mut self) -> bool {
        self.input.peek().is_some()
    }

    fn new(input: Chars<'a>) -> Self {
        Tokenizer {
            input: input.peekable(),
        }
    }

    /// A take method that does not consume the entire iterator.
    fn take(&mut self, n: usize) -> Result<String, Error> {
        let mut output = String::new();

        for _ in 0..n {
            let Some(character) = self.next() else {
                return Err(Error::UnexpectedEOF)
            };

            output.push(character);
        }

        Ok(output)
    }
}

pub fn tokenize(file: String) -> Result<Vec<Token>, Error> {
    // File turned into an iterator for ease of use later on
    let mut file = Tokenizer::new(file.chars());

    // What is returned at the end
    let mut final_out = vec![];

    'a: while let Some(char) = file.next() {
        match char {
            // Comments, or division
            '/' => match file.peek() {
                Some('*') => {
                    while Some('*') != file.next()
                        && Some('/') != file.next()
                        && !file.eof()
                    {}
                }
                Some('/') => while Some('\n') != file.next() && None != file.next() {},
                Some(_) => final_out.push(Token::Math(Math::Div)),
                None => return Err(Error::UnexpectedEOF),
            },
            // Idents
            'a'..='z' | 'A'..='Z' | '_' | '#' | '$' => {
                let mut ident = String::from(char);

                while let Some(chr) = file.next_if(|x| matches!(x, 'a'..='z' | 'A'..='Z' | '_' | '#' | '$') || x.is_alphanumeric()) {
                    ident.push(chr);
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
                    "eq" => Token::Keyword(Keyword::Eq),
                    "ineq" => Token::Keyword(Keyword::Ineq),
                    "is" => Token::Keyword(Keyword::Is),
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
                    "raw#" => Token::Keyword(Keyword::Raw),
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
                        match file.next_or()? {
                            'n' => string.push('\n'),
                            'r' => string.push('\r'),
                            't' => string.push('\t'),
                            '0' => string.push('\0'),
                            'u' => {
                                let hexcode = String::from_iter([
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                ]);

                                let num = u8::from_str_radix(&hexcode, 16)?;

                                string.push(num as char);
                            }
                            'x' => {
                                let hexcode = String::from_iter([
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                    file.next_or()?,
                                ]);

                                let num = u8::from_str_radix(&hexcode, 16)?;

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
            '>' => {
                if let Some('>') = file.peek() {
                    file.next();
                    if let Some('>') = file.peek() {
                        file.next();
                        final_out.push(Token::Math(Math::RZeroFillShift));
                        continue;
                    }
                    final_out.push(Token::Math(Math::RBitShift));
                    continue;
                }
                final_out.push(Token::Math(Math::Gt));
            }
            // Lt
            '<' => {
                if let Some('<') = file.peek() {
                    file.next();
                    final_out.push(Token::Math(Math::LBitShift));
                    continue;
                }
                final_out.push(Token::Math(Math::Lt))
            }
            '\\' => final_out.push(Token::Math(Math::Root)),
            '&' => final_out.push(Token::Math(Math::BitAnd)),
            // Root
            '|' => final_out.push(Token::Math(Math::BitOr)),
            '~' => final_out.push(Token::Math(Math::BitFlip)),
            // Equals
            '=' => final_out.push(Token::Equals),
            // Numbers
            '0'..='9' => {
                let mut num = String::from(char);

                println!("{char} -> {}", file.peek_or()?);

                while let Some(chr) = file.next_if(|x| matches!(x, '0'..='9' | '.')) {
                    if chr == '.' && file.next_eq('.') {
                        let num = Decimal::from_str(&num)?;

                        final_out.push(Token::Num(num));

                        final_out.push(Token::Math(Math::Range));

                        continue 'a;
                    }

                    num.push(chr)
                }

                println!("{num}");

                final_out.push(Token::Num(Decimal::from_str(&num)?));
            }
            // Ranges or field accessors
            '.' => {
                if file.next_eq('.') {
                    file.next();
                    final_out.push(Token::Math(Math::Range))
                } else {
                    final_out.push(Token::Dot);
                }
            }
            // Colon
            ':' => final_out.push(Token::Colon),
            // Macro invocation (e.g macro!)
            '!' => final_out.push(Token::MacroInvocation),
            // Macro variables (e.g @ident)
            '@' => {
                final_out.push(Token::MacroVariable)
            },
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
                    let char = file.next_or()?;

                    if char == '\\' && in_string {
                        src.push(char);
                        src.push(file.next_or()?);
                    }

                    if char == '"' || char == '\'' {
                        in_string = !in_string;
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
                    BlockType::Array => Token::Array(tokenize(src)?),
                    BlockType::Block => Token::Block(tokenize(src)?),
                    BlockType::Group => Token::Group(tokenize(src)?),
                    [_, _] => unreachable!(),
                });
            }
            // Ignore whitespace
            chr if chr.is_whitespace() => {}
            // Unexpected characters
            chr => return Err(Error::Unexpected(Type::Char(chr))),
        }
    }

    Ok(final_out)
}
