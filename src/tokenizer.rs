use std::str::FromStr;

use rust_decimal::Decimal;

#[derive(Debug)]
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
}

#[derive(Debug)]
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

// I know this is a terrible idea. But it's just more convenient!
#[allow(non_snake_case)]
pub mod BlockType {
    #[allow(non_upper_case_globals)]
    pub const Array: [char; 2] = ['[', ']'];
    #[allow(non_upper_case_globals)]
    pub const Block: [char; 2] = ['{', '}'];
    #[allow(non_upper_case_globals)]
    pub const Group: [char; 2] = ['(', ')'];
}

#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),
    Math(Math),
    Equals,
    DotAccessor,
    Ident(String),
    Str(String),
    Bool(bool),
    Num(Decimal),
    Group(Vec<Token>),
    Array(Vec<Token>),
    Block(Vec<Token>),
}


pub fn tokenize(file: String) -> Vec<Token> {
    let mut file = file.chars().peekable();
    let mut final_out = vec![];

    'a: while let Some(char) = file.next() {
        match char {
            '/' => {
                match file.peek() {
                    Some('*') => {
                        while
                        Some('*') != file.next() &&
                            Some('/') != file.next() &&
                            None != file.next() {}
                    },
                    Some('/') => {
                        while Some('\n') != file.next() && None != file.next() {}
                    },
                    Some(_) => final_out.push(Token::Math(Math::Div)),
                    None => panic!("Unexpected EOF")
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::from(char);

                while let Some('a'..='z' | 'A'..='Z' | '_') = file.peek() {
                    ident.push(file.next().unwrap());
                }

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
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    _ => Token::Ident(ident)
                };

                final_out.push(out);
            }
            chr @ '"' | chr @ '\'' => {
                let mut string = String::new();

                while let Some(char) = file.next() {
                    if char == chr {
                        break;
                    }
                    if char == '\\' {
                        string.push(file.next().unwrap());
                        continue
                    }

                    string.push(char);
                }

                final_out.push(Token::Str(string))
            }
            '-' => final_out.push(Token::Math(Math::Sub)),
            '+' => final_out.push(Token::Math(Math::Add)),
            // Terrible approach right here.
            '*' => if Some(&'*') == file.peek() {
                final_out.push(Token::Math(Math::Pow));
                file.next();
            } else {
                final_out.push(Token::Math(Math::Mul))
            },
            '%' => final_out.push(Token::Math(Math::Mod)),
            '^' => final_out.push(Token::Math(Math::Xor)),
            '>' => final_out.push(Token::Math(Math::Gt)),
            '<' => final_out.push(Token::Math(Math::Lt)),
            '~' => final_out.push(Token::Math(Math::Root)),
            '=' => final_out.push(Token::Equals),
            '0'..='9' => {
                let mut num = String::from(char);

                while let Some(chr @ '0'..='9' | chr @ '.') = file.peek() {
                    // Safeguard to stop interpreter from consuming these as part
                    // of the int.
                    if *chr == '.' && file.peek() == Some(&'.') {
                        file.next();
                        file.next();

                        final_out.push(
                            Token::Num(Decimal::from_str(&num).unwrap())
                        );

                        final_out.push(Token::Math(Math::Range));

                        continue 'a;
                    }
                    num.push(file.next().unwrap());
                }

                final_out.push(
                    Token::Num(Decimal::from_str(&num).unwrap())
                );
            }
            '.' => if let Some('.') = file.peek() {
                file.next();
                final_out.push(Token::Math(Math::Range))
            } else {
                final_out.push(Token::DotAccessor);
            },
            bracket @ '[' | bracket @ '(' | bracket @ '{' => {
                let brackets = match bracket {
                    '[' => BlockType::Array,
                    '(' => BlockType::Group,
                    '{' => BlockType::Block,
                    _ => unreachable!()
                };

                let mut src = String::new();

                let mut bracket_count: usize = 1;

                while bracket_count != 0 {
                    let char = file.next().unwrap();

                    if char == brackets[1] {
                        bracket_count -= 1;

                        if bracket_count != 0 {
                            src.push(char);
                        }

                        continue
                    } else if char == brackets[0] {
                        bracket_count += 1;

                        src.push(char);

                        continue
                    }

                    src.push(char);
                }

                final_out.push(
                    match brackets {
                        BlockType::Array => Token::Array(tokenize(src)),
                        BlockType::Block => Token::Block(tokenize(src)),
                        BlockType::Group => Token::Group(tokenize(src)),
                        [_, _] => unreachable!()
                    }
                );
            }
            chr if chr.is_whitespace() => {}
            _ => panic!("Unexpected char {char}! {final_out:?}")
        }
    }

    final_out
}