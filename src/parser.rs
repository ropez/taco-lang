use std::{iter::Peekable, vec::IntoIter};

use crate::lexer::Token;

pub enum AstNode {
    Call {
        subject: Box<str>,
        arguments: Vec<Box<str>>,
    },
}

struct Parser {
    iter: Peekable<IntoIter<Token>>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            iter: tokens.into_iter().peekable(),
        }
    }

    fn parse(&mut self) -> Vec<AstNode> {
        let mut ast = Vec::new();

        loop {
            let Some(token) = self.iter.next() else { break };

            match token {
                Token::NewLine => {
                    // Ignore
                }
                Token::Identifier(name) => {
                    let p = self.iter.next().expect("token after ident");
                    assert!(matches!(p, Token::LeftParen));

                    let arguments = self.parse_argument_list();
                    let node = AstNode::Call {
                        subject: name,
                        arguments,
                    };
                    ast.push(node);

                    let p = self.iter.next().expect("token after ident");
                    assert!(matches!(p, Token::NewLine));
                }
                _ => panic!("Unexpected token: {token:?}"),
            }
        }

        ast
    }

    fn parse_argument_list(&mut self) -> Vec<Box<str>> {
        let mut arguments = Vec::new();

        loop {
            let token = self.iter.next().expect("function argument");
            match token {
                Token::String(s) => arguments.push(s),
                Token::RightParen => break,
                _ => panic!("unexpected token: {token:?}"),
            }
        }

        arguments
    }
}

pub fn parse(tokens: Vec<Token>) -> Vec<AstNode> {
    Parser::new(tokens).parse()
}
