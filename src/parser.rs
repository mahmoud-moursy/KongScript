use std::fmt::Display;

use rust_decimal::{prelude::ToPrimitive, Decimal};

use crate::tokenizer::{Math, Token};

#[derive(Debug, Clone)]
pub enum Node {
    Assign {
        key: Box<Node>,
        value: Box<Node>,
    },
    Let {
        key: Box<Node>,
        value: Box<Node>,
    },
    ObjIdx {
        object: Box<Node>,
        idx: Box<Node>,
    },
    AnonFunc {
        args: Vec<Node>,
        code: Box<Node>,
    },
    FunDecl {
        name: String,
        args: Vec<Node>,
        code: Vec<Node>,
    },
    FnCall(Box<Node>, Vec<Node>),
    Op(Math, Box<Node>, Box<Node>),
    Group(Vec<Node>),
    Block(Vec<Node>),
    Array(Vec<Node>),
    Bool(bool),
    Num(Decimal),
    Str(String),
    FmtStr(String),
    RegexStr(String, String),
    Var(String),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Node::Var(name) => name.to_owned(),
                Node::Assign { key, value } => format!("{key} = {value};"),
                Node::Let { key, value } => format!("let {key} = {value};"),
                Node::ObjIdx { object, idx } => format!("{object}[{idx}]"),
                Node::Bool(b) => format!("{b}"),
                Node::Num(n) =>
                    if n.to_f32().unwrap_or(std::f32::MAX) == std::f32::MAX {
                        format!("{n}n")
                    } else {
                        format!("{n}")
                    },
                Node::Str(st) => format!("{st:?}"),
                Node::FmtStr(st) => format!("`{st}`"),
                Node::RegexStr(st, param) => format!("RegExp.new({st:?}, {param:?})"),
                Node::Group(gr) => format!(
                    "({})",
                    gr.into_iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(",")
                ),
                Node::Array(gr) => format!(
                    "[{}]",
                    gr.into_iter()
                        .fold(String::new(), |acc, val| { acc + &format!("{val},") })
                ),
                Node::Block(gr) => format!(
                    "(()=>{{{}}})()",
                    gr.into_iter()
                        .fold(String::new(), |acc, val| { acc + &format!("{val};") })
                ),
                Node::Op(Math::Range, lhs, rhs) => {
                    if !matches!(lhs, box Node::Num(_)) || !matches!(rhs, box Node::Num(_)) {
                        format!("[...Array({rhs})].map(i => i + {lhs})")
                    } else {
                        let box Node::Num(lhs) = lhs else {
                                                    unreachable!()
                                                };
                        let box Node::Num(rhs) = rhs else {
                                                    unreachable!()
                                                };
                        format!("{:?}", (lhs..rhs))
                    }
                }
                Node::Op(Math::Root, lhs, rhs) => format!("{rhs}**(1/{lhs})"),
                Node::Op(op, lhs, rhs) => format!("{lhs}{op}{rhs}"),
                Node::AnonFunc { args, code } => {
                    format!(
                        "({}) => {}",
                        args.iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(","),
                        code
                    )
                }
                Node::FnCall(name, args) => format!(
                    "{name}{}\n",
                    args.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(",")
                ),
                Node::FunDecl { name, args, code } => {
                    format!(
                        "const {name} = ({}) => {{{}}}",
                        args.iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(","),
                        code.iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(";")
                    )
                }
            }
        )
    }
}

pub fn parse(input: Vec<Token>) -> Vec<Node> {

    let mut input = input.into_iter();

    while let Some(tok) = input.next() {
        // There are no wildcard matches here to ensure that no case is missed
        // and accidentally passed as an "unexpected token".
        match tok {
            Token::Keyword(_) => todo!(),
            Token::Math(m) => panic!("Unexpected token: {m}"),
            Token::Equals => panic!("Unexpected token: ="),
            Token::DotAccessor => panic!("Unexpected token: ."),
            Token::AnonymousArrow => panic!("Unexpected token: ->"),
            Token::Colon => panic!("Unexpected token: :"),
            Token::MacroInvocation => panic!("Unexpected token: !"),
            Token::MacroVariable => panic!("Unexpected token: $!"),
            Token::Ident(_) => todo!(),
            Token::Str(_) => todo!(),
            Token::Bool(_) => todo!(),
            Token::Num(_) => todo!(),
            Token::Group(_) => todo!(),
            Token::Array(_) => todo!(),
            Token::Block(_) => todo!(),
        }
    }

    todo!()
}
