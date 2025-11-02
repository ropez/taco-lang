use std::{iter::Peekable, sync::Arc, vec::IntoIter};

use crate::lexer::Token;

pub enum AstNode {
    Assignment {
        name: Arc<str>,
        value: Expression,
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

    Expression(Expression),
}

// Expressions must have type
#[derive(Debug)]
pub enum Expression {
    Ref(Arc<str>),
    String(Arc<str>),
    List(Vec<Expression>),
    Call {
        subject: Arc<str>,
        arguments: Vec<Expression>,
    },
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
                            let arguments = self.parse_list(Token::RightParen);
                            let node = AstNode::Expression(Expression::Call {
                                subject: name,
                                arguments,
                            });
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

                    ast.push(AstNode::Iteration {
                        ident,
                        iterable,
                        body,
                    });
                }
                _ => panic!("Unexpected token: {token:?}"),
            }
        }

        ast
    }

    fn parse_expression(&mut self) -> Expression {
        let token = self.iter.next().expect("expression");
        match token {
            Token::Identifier(s) => {
                match self.iter.peek() {
                    // FIXME: Call should be a "continuation" of an expression.
                    // The expression can be anything such as a fun returning a fun.
                    Some(Token::LeftParen) => {
                        self.iter.next();
                        let arguments = self.parse_list(Token::RightParen);
                        let node = Expression::Call {
                            subject: s,
                            arguments,
                        };

                        self.expect(Token::NewLine);
                        node
                    }
                    _ => Expression::Ref(s),
                }
            }
            Token::String(s) => Expression::String(s),
            Token::LeftSquare => {
                // FIXME: Infer type of list, and validate each item's type
                // Not sure if this is really a parser concern.
                // Needs to take scope into consideration, so it's probably
                // some analysis phase after parsing, but before evaluation.
                let list = self.parse_list(Token::RightSquare);

                Expression::List(list)
            }
            _ => panic!("unexpected token: {token:?}"),
        }
    }

    fn parse_list(&mut self, until: Token) -> Vec<Expression> {
        let mut arguments = Vec::new();

        // Before: 0-n newline
        // Between: Exactly 1 newline or comma, followed by 0-n newline
        // After: 0-1 comma, followed by 0-n newline

        loop {
            self.consume_whitespace();
            let next = self.iter.peek().expect("token");
            if *next == until {
                self.iter.next();
                break;
            }

            arguments.push(self.parse_expression());

            let token = self.iter.next().expect("token");
            if token == until {
                break;
            } else if token != Token::Comma && token != Token::NewLine {
                panic!("unexpected token: {token:?}")
            }
        }

        arguments
    }

    fn consume_whitespace(&mut self) {
        loop {
            match self.iter.peek() {
                None => break,
                Some(Token::NewLine) => {}
                Some(_) => break,
            }

            self.iter.next();
        }
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
