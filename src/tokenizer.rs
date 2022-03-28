use std::fmt::{Display, Formatter};
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
		macro_rules! match_up {
			($($name: ident),+) => {
				match self {
					$(
						Math::$name => Node::$name(box lhs, box rhs)
					),*
				}
			}
		}

		match_up! {
			Add,
			Sub,
			Mul,
			Div,
			Pow,
			Mod,
			Root,
			Gt,
			Lt,
			Xor,
			Range
		}
	}
}

impl Display for Math {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Math::Add => { "+" }
			Math::Sub => { "-" }
			Math::Mul => { "*" }
			Math::Div => { "/" }
			Math::Pow => { "**" }
			Math::Mod => { "%" }
			Math::Root => { panic!("Impossible for Sqrt!") }
			Math::Gt => { ">" }
			Math::Lt => { "<" }
			Math::Xor => { "^" }
			Math::Range => { panic!("Impossible for range!") }
		})
	}
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

pub mod combinators {
	use chumsky::prelude::*;
	use rust_decimal::Decimal;

	use crate::parser;
	use crate::parser::Node;
	use crate::tokenizer::{Keyword, Math};
	use crate::tokenizer::Token::{self, *};

	pub fn token(t: Token) -> impl Parser<Token, Token, Error=Simple<Token>> {
		filter(move |e| e == &t)
	}

	pub fn keyword(k: Keyword) -> impl Parser<Token, Token, Error=Simple<Token>> {
		filter(move |e| e == &Token::Keyword(k))
	}

	pub fn matchers() -> impl Parser<Token, Node, Error=Simple<Token>> {
		var()
			.then(
				keyword(Keyword::Matches)
			).then(var())
			.map(
				|((lhs, _), rhs)| {
					match (lhs.clone(), rhs.clone()) {
						(
							_,
							Node::RegexString(_, _)
						) => Node::Compiled(format!("{rhs}.test({lhs})")),
						(
							Node::Num(_),
							Node::Range(
								..
							)
						) => Node::Compiled(format!("{lhs} in {rhs}")),
						(_, _) => Node::Compiled(format!("{rhs} == {lhs}"))
					}
				}
			)
	}

	pub fn class_match() -> impl Parser<Token, Node, Error=Simple<Token>> {
		var()
			.then(keyword(Keyword::Matches))
			.then(keyword(Keyword::Class))
			.then(ident().or(dot_accessed()))
			.map(|(((var, _), _), class_name)| {
				Node::Compiled(format!("{var} instanceof {class_name}"))
			})
	}

	pub fn regex_string() -> impl Parser<Token, Node, Error=Simple<Token>> {
		ident_with("r").then(string()).then(
			token(Token::Colon).then(ident()).or_not()
		).map(
			|((_, pat), args)| {
				Node::RegexString(
					pat,
					match args {
						Some((_, s)) => s,
						_ => "".into()
					},
				)
			}
		)
	}

	pub fn math_symbol() -> impl Parser<Token, Math, Error=Simple<Token>> {
		filter(move |e| matches!(e, Token::Math(_)))
			.map(|n| if let Token::Math(m) = n {
				m
			} else {
				unreachable!()
			})
	}

	pub fn ident() -> impl Parser<Token, String, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Ident(_))).map(
			|e| {
				let Ident(id) = e else {
					unreachable!()
				};

				id
			}
		)
	}

	pub fn ident_with<'a>(input: &'a str) -> impl Parser<Token, Token, Error=Simple<Token>> + 'a {
		filter(move |e| e == &Ident(input.into()))
	}

	pub fn num() -> impl Parser<Token, Decimal, Error=Simple<Token>> {
		just(Token::Math(Math::Sub)).repeated().then(filter::<_, _, Simple<Token>>(|e| matches!(e, Num(_))).map(
			|e| {
				let Num(n) = e else {
					unreachable!()
				};

				n
			}
		))
			.foldr(
				|_op, n| n * Decimal::from(-1i64)
			)
	}

	pub fn string() -> impl Parser<Token, String, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Str(_))).map(
			|e| {
				let Str(str) = e else {
					unreachable!()
				};

				str
			}
		)
	}

	pub fn boolean() -> impl Parser<Token, bool, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Bool(_))).map(
			|e| {
				let Bool(b) = e else {
					unreachable!()
				};

				b
			}
		)
	}

	pub fn group() -> impl Parser<Token, Vec<Node>, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Group(_))).map(
			|e| {
				let Group(tok) = e else {
					unreachable!()
				};

				parser().parse(tok).unwrap()
			}
		)
	}

	pub fn array() -> impl Parser<Token, Vec<Node>, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Array(_))).map(
			|e| {
				let Array(tok) = e else {
					unreachable!()
				};

				parser().parse(tok).unwrap()
			}
		)
	}

	pub fn block() -> impl Parser<Token, Vec<Node>, Error=Simple<Token>> {
		filter::<_, _, Simple<Token>>(|e| matches!(e, Block(_))).map(
			|e| {
				let Block(tok) = e else {
					unreachable!()
				};

				parser().parse(tok).unwrap()
			}
		)
	}

	pub fn dot_accessed() -> impl Parser<Token, String, Error=Simple<Token>> {
		ident().then(
			token(Token::DotAccessor).then(ident()).repeated().at_least(1)
		).map(
			|(mut f, e)| {
				let e: Vec<String> = e.into_iter().map(|e| e.1).collect();
				f.push('.');
				f.push_str(&e.join("."));
				f
			}
		)
	}

	pub fn var() -> impl Parser<Token, Node, Error=Simple<Token>> {
		let func_call = dot_accessed().or(ident()).then(group()).map(
			|(name, args)| Node::Compiled(format!("{name}{}\n", Node::Group(args)))
		);

		regex_string().or(
			string().map(Node::Str)
		)
			.or(func_call)
			.or(dot_accessed().map(Node::Var))
			.or(ident().map(Node::Var))
			.or(boolean().map(Node::Bool))
			.or(array().map(Node::Array))
			.or(group().map(Node::Group))
			.or(block().map(Node::Block))
			.or(num().map(Node::Num))
	}
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
					"if" => Token::Keyword(Keyword::If),
				  	"else" => Token::Keyword(Keyword::Else),
				  	"elif" => Token::Keyword(Keyword::Elif),
					"for" => Token::Keyword(Keyword::For),
					"while" => Token::Keyword(Keyword::While),
					"return" => Token::Keyword(Keyword::Return),
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

								let num = u8::from_str_radix(
									&hexcode, 16,
								).unwrap();

								string.push(num as char);
							}
							'x' => {
								let hexcode = String::from_iter([
									file.next().unwrap(),
									file.next().unwrap(),
									file.next().unwrap(),
									file.next().unwrap(),
								]);

								let num = u8::from_str_radix(
									&hexcode, 16,
								).unwrap();

								string.push(num as char);
							}
							a => string.push(a)
						}
						continue
					}
					
					string.push(char);
				}
				
				final_out.push(Token::Str(string))
			}
			'-' => {
			  if Some(&'>') == file.peek() {
				file.next();
				final_out.push(Token::AnonymousArrow);
			  } else {
				final_out.push(Token::Math(Math::Sub));
			  }
			},
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
			// HACK: The whole implementation is kinda hacky.
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
							},
							chr @ '0'..='9' => {
								num.push(dot);
								num.push(*chr);
							}
							any => panic!("Unexpected char `{any}`!")
						}
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
			':' => {
			  final_out.push(Token::Colon)
			}
		  	'!' => final_out.push(Token::MacroInvocation),
		  	'$' => final_out.push(Token::MacroVariable),
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