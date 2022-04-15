use std::fmt::{format, Debug, Display};

use crate::tokenizer::{Keyword, Math, Token};
use chumsky::prelude::*;
use rust_decimal::{prelude::ToPrimitive, Decimal};

#[derive(Debug, Clone)]
pub enum Node {
    Assign { key: Box<Node>, value: Box<Node> },
    Let { key: Box<Node>, value: Box<Node> },
    ObjIdx { object: Box<Node>, idx: Box<Node> },
    AnonFunc { args: Vec<String>, code: Box<Node> },
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
                    if n.to_i32().unwrap_or(std::i32::MAX) >= std::i32::MAX {
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
                    format!("({}) => {}", args.join(","), code)
                }
                Node::FnCall(name, args) => format!("{name}{}\n", Node::Group(args.clone())),
            }
        )
    }
}

macro_rules! combinator {
	($ty: ty) => {
		impl Parser<Token, $ty, Error=Simple<Token>>
	}
}

pub fn ident() -> combinator!(String) {
    let simple_ident = || {
        filter(|e| matches!(e, Token::Ident(_))).map(|e| {
            let Token::Ident(e) = e else {
                unreachable!()
            };

            e
        })
    };

    simple_ident()
        .then(
            just(Token::DotAccessor)
                .then(simple_ident())
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|(mut name, fields)| {
            if let Some(fields) = fields {
                let fields = fields
                    .into_iter()
                    .map(|(_, n)| n)
                    .collect::<Vec<String>>()
                    .join(".");

                name = format!("{name}.{fields}");
            }

            name
        })
}

pub fn string() -> combinator!(String) {
    filter(|e| matches!(e, Token::Str(_))).map(|e| {
        if let Token::Str(e) = e {
            e
        } else {
            unreachable!()
        }
    })
}

pub fn num() -> combinator!(Decimal) {
    filter(|e| matches!(e, Token::Num(_))).map(|e| {
        if let Token::Num(e) = e {
            e
        } else {
            unreachable!()
        }
    })
}

pub fn keyword(kw: Keyword) -> combinator!(Keyword) {
    filter(move |e| e == &Token::Keyword(kw)).map(|e| {
        if let Token::Keyword(kw) = e {
            kw
        } else {
            unreachable!()
        }
    })
}

pub fn operator(m: Math) -> combinator!(Math) {
    filter(move |e| &Token::Math(m) == e).map(|e| {
        if let Token::Math(e) = e {
            e
        } else {
            unreachable!()
        }
    })
}

pub fn any_op() -> combinator!(Math) {
    filter(|e| matches!(e, Token::Math(_))).map(|e| {
        if let Token::Math(m) = e {
            m
        } else {
            unreachable!()
        }
    })
}

pub fn group() -> combinator!(Vec<Node>) {
    filter(|e| matches!(e, Token::Group(_))).map(|e| {
        if let Token::Group(e) = e {
            parser().parse(e).unwrap()
        } else {
            unreachable!()
        }
    })
}

pub fn array() -> combinator!(Vec<Node>) {
    filter(|e| matches!(e, Token::Array(_))).map(|e| {
        if let Token::Array(e) = e {
            parser().parse(e).unwrap()
        } else {
            unreachable!()
        }
    })
}

pub fn block() -> combinator!(Vec<Node>) {
    filter(|e| matches!(e, Token::Block(_))).map(|e| {
        if let Token::Block(e) = e {
            parser().parse(e).unwrap()
        } else {
            unreachable!()
        }
    })
}

pub fn parser() -> combinator!(Vec<Node>) {
    recursive(|expr: Recursive<Token, Node, Simple<Token>>| {
        let array_idx = || {
            ident()
                .map(Node::Var)
                .or(group().map(Node::Group))
                .or(array().map(Node::Array))
                .or(block().map(Node::Block))
                .then(just(Token::Colon))
                .then(
                    expr.clone().then(
                        just(Token::Colon)
                            .repeated()
                            .exactly(2)
                            .ignored()
                            .then(expr.clone())
                            .repeated()
                            .or_not(),
                    ),
                )
                .map(|((value, _), (idx, idx_tree))| {
                    let mut out: Node = Node::ObjIdx {
                        object: box value,
                        idx: box idx,
                    };

                    if let Some(idx_tree) = idx_tree {
                        for (_, idx) in idx_tree {
                            out = Node::ObjIdx {
                                object: box out,
                                idx: box idx,
                            }
                        }
                    }

                    out
                })
        };

        let assign = || {
            ident()
                .map(Node::Var)
                .or(array_idx())
                .then(just(Token::Equals))
                .then(expr.clone())
                .map(|((key, _), value)| Node::Assign {
                    key: box key,
                    value: box value,
                })
        };

        let let_stmt = keyword(Keyword::Let).then(assign()).map(|(_, assignment)| {
            if let Node::Assign { key, value } = assignment {
                Node::Let { key, value }
            } else {
                unreachable!()
            }
        });

        let math = array_idx()
            .or(ident().map(Node::Var))
            .or(num().map(Node::Num))
            .or(group().map(Node::Group))
            .or(block().map(Node::Block))
            .then(any_op())
            .then(expr.clone())
            .map(|((lhs, op), rhs)| op.apply(lhs, rhs));

        let anon_fun = ident()
            .map(|e| vec![Node::Var(e)])
            .or(group())
            .then(just(Token::AnonymousArrow))
            .then(expr.clone())
            .map(|((args, _), code)| {
                let args: Vec<String> = args
                    .into_iter()
                    .map(|e| {
                        if let Node::Var(e) = e {
                            e
                        } else {
                            panic!("Unexpected token in anonymous function declaration")
                        }
                    })
                    .collect();

                Node::AnonFunc {
                    args,
                    code: box code,
                }
            });

        let fun_call = ident()
            .map(Node::Var)
            .or(array_idx())
            .then(group())
            .map(|(name, args)| Node::FnCall(box name, args));

        assign()
            .or(let_stmt)
            .or(anon_fun)
            .or(fun_call)
            .or(math)
            .or(array_idx())
            .or(ident().map(Node::Var))
            .or(string().map(Node::Str))
            .or(block().map(Node::Block))
            .or(group().map(Node::Group))
            .or(array().map(Node::Array))
            .or(num().map(Node::Num))
    })
    .repeated()
    .then_ignore(end())
}
