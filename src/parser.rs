use std::fmt::{Display, Formatter};

use chumsky::prelude::*;
use rust_decimal::Decimal;
use rust_decimal::prelude::ToPrimitive;

use Node::*;

use crate::tokenizer::{Keyword, Math, Token};
use crate::tokenizer::combinators::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
	Add(Box<Node>, Box<Node>),
	Sub(Box<Node>, Box<Node>),
	Mul(Box<Node>, Box<Node>),
	Div(Box<Node>, Box<Node>),
	Gt(Box<Node>, Box<Node>),
	Lt(Box<Node>, Box<Node>),
	Pow(Box<Node>, Box<Node>),
	Mod(Box<Node>, Box<Node>),
	Root(Box<Node>, Box<Node>),
	Range(Box<Node>, Box<Node>),
	Xor(Box<Node>, Box<Node>),
	Num(Decimal),
	Var(String),
	Return(Box<Node>),
	Str(String),
	Array(Vec<Node>),
	Group(Vec<Node>),
	Block(Vec<Node>),
	Bool(bool),
	RegexString(String, String),
	Let {
		name: String,
		value: Box<Node>,
	},
	Assign {
		name: String,
		value: Box<Node>,
	},
	MathEq {
		name: String,
		op: Math,
		value: Box<Node>,
	},
	If {
		condition: Box<Node>,
		code: Vec<Node>,
	},
	Elif {
		condition: Box<Node>,
		code: Vec<Node>,
	},
	Else {
		code: Vec<Node>,
	},
	While {
		condition: Box<Node>,
		code: Vec<Node>,
	},
	FunctionDecl {
		name: String,
		args: Vec<String>,
		body: Vec<Node>,
	},
	Object(Vec<(Node, Node)>),
	AnonymousArrow,
	Compiled(String),
	Stray(Token)
}

impl Display for Node {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Add(lhs, rhs) => write!(f, "{lhs}+{rhs}"),
			Sub(lhs, rhs) => write!(f, "{lhs}-{rhs}"),
			Mul(lhs, rhs) => write!(f, "{lhs}*{rhs}"),
			Div(lhs, rhs) => write!(f, "{lhs}/{rhs}"),
			Gt(lhs, rhs) => write!(f, "{lhs}>{rhs}"),
			Lt(lhs, rhs) => write!(f, "{lhs}<{rhs}"),
			Pow(lhs, rhs) => write!(f, "{lhs}**{rhs}"),
			Mod(lhs, rhs) => write!(f, "{lhs}%{rhs}"),
			Xor(lhs, rhs) => write!(f, "{lhs}^{rhs}"),
			Root(lhs, rhs) => write!(f, "Math.abs({rhs}) ** (1/{lhs})"),
			Node::Str(str) => write!(f, "{str:?}"),
			Range(
				box Node::Num(lhs),
				box Node::Num(rhs)
			) => write!(f, "{:?}", (lhs.to_i64().unwrap()..rhs.to_i64().unwrap()).collect::<Vec<i64>>()),
			Range(
				box lhs,
				box rhs
			) => write!(f, "Array.from({{{rhs}-{lhs}}},(_,i)=>{lhs}+i);"),
			Node::Num(n) => write!(f, "{n}"),
			Node::Var(v) => write!(f, "{v}"),
			Node::Return(out) => write!(f, "return {out};"),
			RegexString(pat, args) => write!(f, "new RegExp({pat:?},{args:?})"),
			Node::Let {
				name,
				value
			} => {
				write!(f, "let {name}={value};")
			}
			Node::Assign {
				name,
				value
			} => {
				write!(f, "{name}={value};")
			}
			Node::Block(g) => {
				write!(f, "(()=>{{{}}})();", g.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(""))
			}
			Node::Array(n) => {
				write!(f, "[{}]", n.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(","))
			}
			Node::Group(g) => {
				write!(f, "({})", g.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(","))
			}
			Node::Bool(b) => write!(f, "{b}"),
			Node::If {
				condition,
				code
			} => write!(f, "if({condition}){}", Node::Block(code.clone())),
			Node::Elif {
				condition,
				code
			} => {
				write!(f, "else if({condition}){}", Node::Block(code.clone()))
			}
			Node::Else {
				code
			} => {
				write!(f, "else {}", Node::Block(code.clone()))
			}
			Node::While {
				condition,
				code
			} => write!(f, "while({condition}){}", Node::Block(code.clone())),
			Node::MathEq {
				name,
				op,
				value,
			} => {
				match op {
					Math::Root | Math::Range => {
						write!(f, "{}", Node::Assign {
							name: name.clone(),
							value: box op.apply(Node::Var(name.clone()), *value.clone()),
						})
					}
					_ => write!(f, "{name}{op}={value};")
				}
			}
			Node::AnonymousArrow => write!(f, "=>"),
			Node::FunctionDecl {
				name,
				args,
				body
			} => write!(
				f, "function {name}({}){{{}}};", args.join(","), body.iter().map(|e| e.to_string()).collect::<Vec<String>>().join("")
			),

			Node::Compiled(any) => write!(f, "{any}"),
			Node::Stray(tok) => write!(f, "{tok}")
		}
	}
}

//noinspection ALL
pub fn parser() -> impl Parser<Token, Vec<Node>, Error=Simple<Token>> {
	recursive(|expr| {
		let r#return = keyword(Keyword::Return)
			.then(expr.clone())
			.map(|(_, e)| Node::Return(box e));

		let assign =
			ident()
				.then(token(Token::Equals))
				.then(expr.clone())
				.map(|((name, _), value)| {
					Node::Assign {
						name,
						value: box value,
					}
				});

		let r#let =
			keyword(Keyword::Let)
				.then(ident())
				.then(
					token(Token::Colon).then(var()).or_not().ignored()
				)
				.then(token(Token::Equals))
				.then(expr.clone())
				.map(|((((_, name), _), _), value)| {
					Node::Let {
						name,
						value: box value,
					}
				});

		let math = var().then(
			math_symbol()
		).then(
			expr.clone()
		).map(
			|((lhs, op), rhs)| {
				op.apply(lhs, rhs)
			}
		);

		let math_eq = (ident())
			.then(math_symbol())
			.then(token(Token::Equals))
			.then(expr.clone())
			.map(
				|(((lhs, op), _), rhs)| {
					Node::MathEq {
						name: lhs,
						op,
						value: box rhs,
					}
				}
			);


		let r#if = keyword(Keyword::If)
			.then(expr.clone())
			.then(block())
			.map(|((_, condition), code)| {
				Node::If {
					condition: box condition,
					code: code,
				}
			});

		let elif = keyword(Keyword::Elif)
			.then(expr.clone())
			.then(block())
			.map(|((_, condition), code)| {
				Node::Elif {
					condition: box condition,
					code: code,
				}
			});

		let r#while = keyword(Keyword::While)
			.then(expr.clone())
			.then(block())
			.map(|((_, condition), code)| {
				Node::While {
					condition: box condition,
					code: code,
				}
			});


		let array_idx = var()
			.then(token(Token::Colon).then(expr.clone()).repeated().at_least(1))
			.map(|(value, idx)| {
				let idx: Vec<String> = idx.into_iter().map(|(_, v)|
					"[".to_owned() + &v.to_string() + "]"
				).collect();
				Node::Compiled(format!("{value}{}", idx.join("")))
			});

		let double_idx = var()
			.then(token(Token::Colon).then(expr.clone()).repeated().at_least(1))
			.map(|(value, idx)| {
				let idx: Vec<String> = idx.into_iter().map(|(_, v)|
					"[".to_owned() + &v.to_string() + "]"
				).collect();
				Node::Compiled(format!("{value}{}", idx.join("")))
			})
			.then(
				token(Token::Colon)
					.then(token(Token::Colon))
					.then(expr.clone())
					.map(
						|((_, _), idx)| {
							idx
						}
					)
					.repeated()
					.at_least(1)
			).map(
			|(value, idx)| {
				let idx: String = idx.into_iter().map(|e| format!("[{e}]")).collect();

				Node::Compiled(format!("{value}{idx}"))
			}
		);

		let fun = keyword(Keyword::Fun)
			.then(ident())
			.then(group())
			.then(
				token(Token::Colon).then(ident()).or_not().ignored()
			)
			.then(block())
			.map(|((((_, name), args), _), body)| {
				Node::FunctionDecl {
					name,
					args: args.into_iter().map(
						|e| {
							if let Node::Var(e) = e {
								e
							} else {
								panic!("Unexpected token in function args!")
							}
						}
					).collect(),
					body,
				}
			});


		let anon_fun = group().then(token(Token::AnonymousArrow)).then(expr.clone()).map(
			|((args, _), body)| {
				Node::Compiled(
					format!("{}=>{body}", Node::Group(args))
				)
			}
		);

		let match_stmt = keyword(Keyword::Match)
			.then(expr.clone())
			.then(
				block()
			).map(
			|((_, expr), block)| {
				let mut block = block.into_iter();

				let mut final_out = String::new();

				while let Some(node) = block.next() {
					let Some(Node::Block(to_eval)) = block.next() else {
						panic!("Unexpected token in match statement!")
					};

					if Node::Var("default".into()) == node {
						final_out.extend(
							format!("default:{}", Node::Block(to_eval)).chars()
						);

						continue;
					}

					let compiled = compile_match(expr.clone(), node);

					final_out.extend(
						format!("case {compiled}:{};break;", Node::Block(to_eval)).chars()
					)
				}

				Node::Compiled(format!("switch(true) {{{final_out}}}"))
			}
		);

		let r#else = keyword(Keyword::Else)
			.then(block())
			.map(|(_, code)| {
				Node::Else {
					code
				}
			});

		r#return
			.or(r#let)
			.or(assign)
			.or(math_eq)
			.or(math)
			.or(double_idx)
			.or(array_idx)
			.or(class_match())
			.or(matchers())
			.or(match_stmt)
			.or(r#else)
			.or(elif)
			.or(r#if)
			.or(r#while)
			.or(anon_fun)
			.or(fun)
			.or(var())
			.or(token(Token::AnonymousArrow).to(Node::AnonymousArrow))
			.or(any().map(Node::Stray))
	})
		.repeated()
		.then_ignore(end())
}

