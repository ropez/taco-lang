use std::{iter::Peekable, sync::Arc, vec::IntoIter};

use crate::lexer::Token;

pub enum AstNode {
    Assignment {
        name: Arc<str>,
        value: Expression,
    },
    Call {
        subject: Arc<str>,
        arguments: Vec<Expression>,
    },

    // Are functions expressions?
    // Is this just an assignment?
    Function {
        name: Arc<str>,
        fun: Arc<Function>,
    },

    Iteration {
        ident: Arc<str>,
        iterable: Expression,
        body: Vec<AstNode>,
    },
}

// Expressions must have type
#[derive(Debug)]
pub enum Expression {
    Ref(Arc<str>),
    String(Arc<str>),
    List(Vec<Expression>),
}

pub struct Function {
    // return type
    // parameters
    // closure
    pub(crate) body: Vec<AstNode>,
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
                Token::NewLine => {}        // Ignore
                Token::RightBrace => break, // FIXME Not allowed at global scope
                Token::Fun => {
                    let name = self.expect_ident();
                    self.expect(Token::LeftParen);
                    // TODO parameters
                    self.expect(Token::RightParen);
                    self.expect(Token::LeftBrace);
                    self.expect(Token::NewLine);

                    let body = self.parse();

                    let fun = Arc::new(Function { body });
                    ast.push(AstNode::Function { name, fun });
                }
                Token::Identifier(name) => {
                    let p = self.iter.next().expect("token after ident");

                    match p {
                        Token::Assign => {
                            let value = self.parse_expression();
                            let node = AstNode::Assignment { name, value };
                            ast.push(node);
                            self.expect(Token::NewLine);
                        }
                        Token::LeftParen => {
                            let arguments = self.parse_argument_list();
                            let node = AstNode::Call {
                                subject: name,
                                arguments,
                            };
                            ast.push(node);

                            self.expect(Token::NewLine);
                        }
                        _ => panic!("Expected assignment or call"),
                    }
                }
                Token::For => {
                    let ident = self.expect_ident();
                    self.expect(Token::In);
                    let iterable = self.parse_expression();
                    self.expect(Token::LeftBrace);
                    let body = self.parse();

                    ast.push(AstNode::Iteration { ident, iterable, body });
                }
                _ => panic!("Unexpected token: {token:?}"),
            }
        }

        ast
    }

    fn parse_expression(&mut self) -> Expression {
        let token = self.iter.next().expect("expression");
        match token {
            Token::Identifier(s) => Expression::Ref(s),
            Token::String(s) => Expression::String(s),
            Token::LeftSquare => {
                let mut list = Vec::new();

                // FIXME: Infer type of list, and each item
                loop {
                    match self.iter.next() {
                        None => panic!("Unexpected end of input"),
                        Some(t) => match t {
                            Token::RightSquare => break,
                            Token::NewLine => {} // Ignore
                            Token::String(s) => list.push(Expression::String(s)),
                            _ => panic!("Unexpected token in list: {t:?}"),
                        }
                    }
                }

                Expression::List(list)
            }
            _ => panic!("unexpected token: {token:?}"),
        }
    }

    fn parse_argument_list(&mut self) -> Vec<Expression> {
        let mut arguments = Vec::new();

        loop {
            let token = self.iter.next().expect("function argument");
            match token {
                Token::NewLine => {} // Ignore
                Token::Identifier(s) => arguments.push(Expression::Ref(s)),
                Token::String(s) => arguments.push(Expression::String(s)),
                Token::RightParen => break,
                _ => panic!("unexpected token: {token:?}"),
            }
        }

        arguments
    }

    fn expect(&mut self, token: Token) {
        let p = self.iter.next().expect("token");
        assert_eq!(p, token, "Expected {token:?}, found {p:?}");
    }

    fn expect_ident(&mut self) -> Arc<str> {
        let p = self.iter.next().expect("token");
        let Token::Identifier(name) = p else {
            panic!("Expected identifier");
        };
        name
    }
}

pub fn parse(tokens: Vec<Token>) -> Vec<AstNode> {
    Parser::new(tokens).parse()
}
