use std::{iter::Peekable, sync::Arc, vec::IntoIter};

use crate::lexer::Token;

#[derive(Debug)]
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

    Condition {
        cond: Expression,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
    },

    Expression(Expression),
}

// Expressions must have type
#[derive(Debug)]
pub enum Expression {
    Ref(Arc<str>),
    String(Arc<str>),
    List(Vec<Expression>),
    Not(Arc<Expression>),
    Equal(Arc<Expression>, Arc<Expression>),
    NotEqual(Arc<Expression>, Arc<Expression>),
    Call {
        subject: Arc<str>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub struct Function {
    // return type
    // parameters
    // closure
    pub(crate) params: Vec<Arc<str>>,
    pub(crate) body: Vec<AstNode>,
}

struct Parser {
    iter: Peekable<IntoIter<Token>>,
}

mod constants {
    pub(crate) const BP_EQUAL: u32 = 1;
    // pub(crate) const BP_INEQUAL: u32 = 2;
    // pub(crate) const BP_PLUS: u32 = 10;
    // pub(crate) const BP_MINUS: u32 = 10;
    // pub(crate) const BP_DIV: u32 = 20;
    // pub(crate) const BP_MULT: u32 = 20;
    // pub(crate) const BP_UNARY: u32 = 30;
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
                    let params = self.parse_params(Token::RightParen);
                    self.expect(Token::LeftBrace);
                    self.expect(Token::NewLine);

                    let body = self.parse();

                    let fun = Arc::new(Function { body, params });
                    ast.push(AstNode::Function { name, fun });
                }
                Token::Identifier(name) => {
                    let p = self.iter.next().expect("token after ident");

                    match p {
                        Token::Assign => {
                            let value = self.parse_expression(0);
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
                Token::If => {
                    let cond = self.parse_expression(0);
                    self.expect(Token::LeftBrace);
                    let body = self.parse();

                    let else_body = self.iter.next_if_eq(&Token::Else).map(|_| {
                        self.expect(Token::LeftBrace);
                        self.parse()
                    });

                    ast.push(AstNode::Condition {
                        cond,
                        body,
                        else_body,
                    });
                }
                Token::For => {
                    let ident = self.expect_ident();
                    self.expect(Token::In);
                    let iterable = self.parse_expression(0);
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

    fn parse_expression(&mut self, bp: u32) -> Expression {
        let token = self.iter.next().expect("expression");
        match token {
            Token::Identifier(s) => {
                match self.iter.peek() {
                    // FIXME: Call should be a "continuation" of an expression.
                    // The expression can be anything such as a fun returning a fun.
                    Some(Token::LeftParen) => {
                        self.iter.next();
                        let arguments = self.parse_list(Token::RightParen);
                        Expression::Call {
                            subject: s,
                            arguments,
                        }
                    }
                    _ => self.parse_continuation(Expression::Ref(s), bp),
                }
            }
            Token::Not => {
                let expr = self.parse_expression(0);
                Expression::Not(expr.into())
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

    fn parse_continuation(&mut self, lhs: Expression, bp: u32) -> Expression {
        use constants::*;

        match self.iter.peek() {
            None => lhs,
            Some(token) => match token {
                Token::Equal => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL);
                        let expr = Expression::Equal(lhs.into(), rhs.into());
                        self.parse_continuation(expr, bp)
                    }
                }
                Token::NotEqual => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL);
                        let expr = Expression::NotEqual(lhs.into(), rhs.into());
                        self.parse_continuation(expr, bp)
                    }
                }
                _ => lhs
            }
        }
    }

    fn parse_params(&mut self, until: Token) -> Vec<Arc<str>> {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let next = self.iter.peek().expect("token");
            if *next == until {
                self.iter.next();
                break;
            }

            items.push(self.expect_ident());

            let token = self.iter.next().expect("token");
            if token == until {
                break;
            } else if token != Token::Comma && token != Token::NewLine {
                panic!("unexpected token: {token:?}")
            }
        }

        items
    }

    fn parse_list(&mut self, until: Token) -> Vec<Expression> {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let next = self.iter.peek().expect("token");
            if *next == until {
                self.iter.next();
                break;
            }

            items.push(self.parse_expression(0));

            let token = self.iter.next().expect("token");
            if token == until {
                break;
            } else if token != Token::Comma && token != Token::NewLine {
                panic!("unexpected token: {token:?}")
            }
        }

        items
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
