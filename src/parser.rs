use std::iter::Peekable;

use crate::errors::Error;
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

impl Node {
    pub(crate) fn compile(&self) -> String {
        match self {
            Node::Var(name) => name.to_owned(),
            Node::Assign { key, value } => format!("{} = {};", key.compile(), value.compile()),
            Node::Let { key, value } => format!("let {} = {};", key.compile(), value.compile()),
            Node::ObjIdx { object, idx } => format!("{}[{}]", object.compile(), idx.compile()),
            Node::Bool(b) => format!("{b}"),
            Node::Num(n) =>
            // Decide if number should be a BigInt or a regular int
            {
                if n.to_f32().unwrap_or(f32::MAX) == f32::MAX {
                    format!("{n}n")
                } else {
                    format!("{n}")
                }
            }
            Node::Str(st) => format!("{st:?}"),
            Node::FmtStr(st) => format!("`{st}`"),
            Node::RegexStr(st, param) => format!("RegExp.new({st:?}, {param:?})"),
            Node::Group(gr) => format!(
                "({})",
                gr.iter()
                    .map(Node::compile)
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Node::Array(arr) => format!(
                "[{}]",
                arr.iter().fold(String::new(), |acc, val| {
                    acc + &format!("{},", val.compile())
                })
            ),
            Node::Block(block) => format!(
                "{{{}}}",
                block.iter().fold(String::new(), |acc, val| {
                    acc + &format!("{};", val.compile())
                })
            ),
            Node::Op(Math::Range, lhs, rhs) => {
                if !matches!(lhs, box Node::Num(_)) || !matches!(rhs, box Node::Num(_)) {
                    format!(
                        "[...Array({})].map(i => i + {})",
                        rhs.compile(),
                        lhs.compile()
                    )
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
            Node::Op(Math::Root, lhs, rhs) => format!("{}**(1/{})", rhs.compile(), lhs.compile()),
            Node::Op(op, lhs, rhs) => format!("{}{}{}", lhs.compile(), op, rhs.compile()),
            Node::AnonFunc { args, code } => {
                format!(
                    "({}) => {}",
                    args.iter()
                        .map(Node::compile)
                        .collect::<Vec<String>>()
                        .join(","),
                    code.compile()
                )
            }
            Node::FnCall(name, args) => format!(
                "{}{}\n",
                name.compile(),
                args.iter()
                    .map(Node::compile)
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Node::FunDecl { name, args, code } => {
                format!(
                    "const {name} = ({}) => {{{}}}",
                    args.iter()
                        .map(Node::compile)
                        .collect::<Vec<String>>()
                        .join(","),
                    code.iter()
                        .map(Node::compile)
                        .collect::<Vec<String>>()
                        .join(";")
                )
            }
        }
    }
}

pub fn parse(input: Vec<Token>) -> Vec<Node> {
    let mut input = input.into_iter();

    let mut final_out: Vec<Node> = Vec::new();

    while let Some(tok) = input.next() {
        // There are no wildcard matches here to ensure that no case is missed
        // and accidentally passed as an "unexpected token".
    }

    final_out
}

pub struct Parser {
    input: Vec<Token>,
    last: Option<Token>
}
