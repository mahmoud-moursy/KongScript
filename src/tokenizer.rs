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
    Export
}

#[repr(u8)]
pub enum Math {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Root
}



pub enum Token {
    Keyword(Keyword),
    Math(Math),
    Ident(String),
    Bool(bool),
    Num(isize),
    Float(isize),
    Group(Vec<Token>),
    Array(Vec<Token>)
}

pub fn tokenize(_file: String) {

}