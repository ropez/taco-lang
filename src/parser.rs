use std::{iter::Peekable, sync::Arc, vec::IntoIter};

use crate::lexer::Token;

pub enum AstNode {
    Assignment {
        name: Arc<str>,
        value: Box<str>, // TODO Expression
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
}

// Expressions must have type
pub enum Expression {
    Ref(Arc<str>),
    String(Box<str>),
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
                    let p = self.iter.next().expect("token after fun");
                    let Token::Identifier(name) = p else {
                        panic!("Expected identifier");
                    };
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
                            let v = self.iter.next().expect("assignment value");
                            let Token::String(s) = v else {
                                panic!("Expected string");
                            };
                            let node = AstNode::Assignment { name, value: s };
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
                _ => panic!("Unexpected token: {token:?}"),
            }
        }

        ast
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
}

pub fn parse(tokens: Vec<Token>) -> Vec<AstNode> {
    Parser::new(tokens).parse()
}
