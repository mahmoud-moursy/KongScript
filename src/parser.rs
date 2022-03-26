use rust_decimal::Decimal;
use crate::tokenizer::Token;

pub enum Node {
	Add(Decimal, Decimal),
  	Sub(Decimal, Decimal),
  	Mul(Decimal, Decimal),
  	Div(Decimal, Decimal),
  	FunctionCall {
	  name: Vec<String>,
	  args: Vec<Node>,
	},
  	Return(Node),
}

pub fn parse(tokens: Vec<Token>) {
	let mut final_out: Vec<Node> = vec![];
  	let mut tokens = tokens.into_iter().peekable();
  	
  	while let Some(tok) = tokens.next() {
	  match tok {
		Token::Keyword(_) => todo!(),
		Token::Math(_) => todo!(),
		Token::Equals => todo!(),
		Token::DotAccessor => todo!(),
		Token::AnonymousArrow => todo!(),
		Token::Colon => todo!(),
		Token::MacroInvocation => todo!(),
		Token::MacroVariable => todo!(),
		Token::Ident(_) => todo!(),
		Token::Str(_) => todo!(),
		Token::Bool(_) => todo!(),
		Token::Num(_) => todo!(),
		Token::Group(_) => todo!(),
		Token::Array(_) => todo!(),
		Token::Block(_) => todo!(),
	  }
	}
}