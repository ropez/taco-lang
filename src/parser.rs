use std::{iter::Peekable, sync::Arc, vec::IntoIter};

use crate::{
    interp::{self, StringToken},
    lexer::{self, Token, TokenKind},
};

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

    Record(Arc<Record>),

    Expression(Expression),
    Return(Expression),
}

// Expressions must have type
#[derive(Debug)]
pub enum Expression {
    Ref(Arc<str>),
    String(Arc<str>),
    StringInterpolate(Vec<Expression>),
    Number(i64),
    List(Vec<Expression>),
    Not(Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    Range(Box<Expression>, Box<Expression>),
    Call {
        subject: Arc<str>,
        args: Vec<Expression>,
        kwargs: Vec<(Arc<str>, Expression)>,
    },
    Access {
        subject: Box<Expression>,
        key: Arc<str>,
    },
}

#[derive(Debug)]
pub struct Function {
    // return type
    // parameters
    pub(crate) params: Vec<Arc<str>>,
    pub(crate) body: Vec<AstNode>,
}

#[derive(Debug)]
pub struct Record {
    pub(crate) name: Arc<str>,
    pub(crate) params: Vec<Arc<str>>,
}

struct Parser {
    iter: Peekable<IntoIter<Token>>,
}

mod constants {
    pub(crate) const BP_EQUAL: u32 = 1;
    // pub(crate) const BP_INEQUAL: u32 = 2;
    pub(crate) const BP_SPREAD: u32 = 6;
    // pub(crate) const BP_PLUS: u32 = 10;
    // pub(crate) const BP_MINUS: u32 = 10;
    // pub(crate) const BP_DIV: u32 = 20;
    // pub(crate) const BP_MULT: u32 = 20;
    // pub(crate) const BP_UNARY: u32 = 30;
    pub(crate) const BP_CALL: u32 = 90;
    pub(crate) const BP_ACCESS: u32 = 100;
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

            match token.kind {
                TokenKind::NewLine => {}        // Ignore
                TokenKind::RightBrace => break, // FIXME Not allowed at global scope
                TokenKind::Fun => {
                    let name = self.expect_ident();
                    self.expect(TokenKind::LeftParen);
                    let params = self.parse_params(TokenKind::RightParen);
                    self.expect(TokenKind::LeftBrace);
                    self.expect(TokenKind::NewLine);

                    let body = self.parse();

                    let fun = Arc::new(Function { body, params });
                    ast.push(AstNode::Function { name, fun });
                }
                // FIXME Not allowed at global scope
                TokenKind::Return => {
                    let expr = self.parse_expression(0);
                    ast.push(AstNode::Return(expr));
                }
                TokenKind::Identifier(name) => {
                    let p = self.iter.next().expect("token after ident");

                    match p.kind {
                        TokenKind::Assign => {
                            let value = self.parse_expression(0);
                            let node = AstNode::Assignment { name, value };
                            ast.push(node);
                            self.expect(TokenKind::NewLine);
                        }
                        TokenKind::LeftParen => {
                            let (args, kwargs) = self.parse_args();
                            let node = AstNode::Expression(Expression::Call {
                                subject: name,
                                args,
                                kwargs,
                            });
                            ast.push(node);

                            self.expect(TokenKind::NewLine);
                        }
                        _ => panic!("Expected assignment or call"),
                    }
                }
                TokenKind::If => {
                    let cond = self.parse_expression(0);
                    self.expect(TokenKind::LeftBrace);
                    let body = self.parse();

                    let else_body = self.next_if_kind(&TokenKind::Else).map(|_| {
                        self.expect(TokenKind::LeftBrace);
                        self.parse()
                    });

                    ast.push(AstNode::Condition {
                        cond,
                        body,
                        else_body,
                    });
                }
                TokenKind::For => {
                    let ident = self.expect_ident();
                    self.expect(TokenKind::In);
                    let iterable = self.parse_expression(0);
                    self.expect(TokenKind::LeftBrace);
                    let body = self.parse();

                    ast.push(AstNode::Iteration {
                        ident,
                        iterable,
                        body,
                    });
                }
                TokenKind::Record => {
                    let ident = self.expect_ident();
                    self.expect(TokenKind::LeftParen);
                    let params = self.parse_params(TokenKind::RightParen);

                    let rec = Arc::new(Record {
                        name: ident,
                        params,
                    });
                    ast.push(AstNode::Record(rec))
                }
                _ => panic!("Unexpected token: {token:?}"),
            }
        }

        ast
    }

    fn parse_expression(&mut self, bp: u32) -> Expression {
        let token = self.iter.next().expect("expression");
        match token.kind {
            TokenKind::Identifier(s) => {
                self.parse_continuation(Expression::Ref(s), bp)
            }
            TokenKind::Not => {
                let expr = self.parse_expression(bp);
                Expression::Not(expr.into())
            }
            TokenKind::String(s) => {
                let expr = parse_string(s);
                self.parse_continuation(expr, bp)
            }
            TokenKind::Number(n) => {
                let e = Expression::Number(n);
                self.parse_continuation(e, bp)
            }
            TokenKind::LeftSquare => {
                // FIXME: Infer type of list, and validate each item's type
                // Not sure if this is really a parser concern.
                // Needs to take scope into consideration, so it's probably
                // some analysis phase after parsing, but before evaluation.
                let list = self.parse_list(TokenKind::RightSquare);

                Expression::List(list)
            }
            _ => panic!("unexpected token: {token:?}"),
        }
    }

    fn parse_continuation(&mut self, lhs: Expression, bp: u32) -> Expression {
        use constants::*;

        match self.peek_kind() {
            None => lhs,
            Some(token) => match token {
                TokenKind::Equal => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL);
                        let expr = Expression::Equal(lhs.into(), rhs.into());
                        self.parse_continuation(expr, bp)
                    }
                }
                TokenKind::NotEqual => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL);
                        let expr = Expression::NotEqual(lhs.into(), rhs.into());
                        self.parse_continuation(expr, bp)
                    }
                }
                TokenKind::Dot => {
                    if bp >= BP_ACCESS {
                        lhs
                    } else {
                        self.iter.next();
                        let key = self.expect_ident();
                        let expr = Expression::Access {
                            subject: lhs.into(),
                            key,
                        };
                        self.parse_continuation(expr, bp)
                    }
                }
                TokenKind::Spread => {
                    if bp >= BP_SPREAD {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_SPREAD);
                        let expr = Expression::Range(lhs.into(), rhs.into());
                        self.parse_continuation(expr, bp)
                    }
                }
                TokenKind::LeftParen => {
                    if bp >= BP_CALL {
                        lhs
                    } else {
                        self.iter.next();

                        // XXX Change Call subject to Expression
                        let Expression::Ref(subject) = lhs else {
                            panic!("Unespected call on {lhs:?}");
                        };

                        let (args, kwargs) = self.parse_args();

                        Expression::Call {
                            subject,
                            args,
                            kwargs,
                        }
                    }
                }
                _ => lhs,
            },
        }
    }

    fn parse_params(&mut self, until: TokenKind) -> Vec<Arc<str>> {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let next = self.iter.peek().expect("token");
            if next.kind == until {
                self.iter.next();
                break;
            }

            items.push(self.expect_ident());

            let token = self.iter.next().expect("token");
            if token.kind == until {
                break;
            } else if token.kind != TokenKind::Comma && token.kind != TokenKind::NewLine {
                panic!("unexpected token: {token:?}")
            }
        }

        items
    }

    fn parse_list(&mut self, until: TokenKind) -> Vec<Expression> {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let next = self.iter.peek().expect("token");
            if next.kind == until {
                self.iter.next();
                break;
            }

            items.push(self.parse_expression(0));

            let token = self.iter.next().expect("token");
            if token.kind == until {
                break;
            } else if token.kind != TokenKind::Comma && token.kind != TokenKind::NewLine {
                panic!("unexpected token: {token:?}")
            }
        }

        items
    }

    fn parse_args(&mut self) -> (Vec<Expression>, Vec<(Arc<str>, Expression)>) {
        let mut args = Vec::new();
        let mut kwargs = Vec::new();

        loop {
            self.consume_whitespace();
            if self.next_if_kind(&TokenKind::RightParen).is_some() {
                break;
            }

            // TODO Checks needed here:
            // positional args must come before kwargs
            //
            // Can check here or later:
            // `name` is only assigned once
            //
            // Must check later:
            // no kwargs for argument assigned positionally

            if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
                let name = Arc::clone(name);
                self.iter.next();

                if self.next_if_kind(&TokenKind::Colon).is_some() {
                    let value = self.parse_expression(0);
                    kwargs.push((name, value));
                } else {
                    let value = self.parse_continuation(Expression::Ref(name), 0);
                    args.push(value);
                }
            } else {
                let value = self.parse_expression(0);
                args.push(value);
            }

            let token = self.iter.next().expect("token");

            if token.kind == TokenKind::RightParen {
                break;
            } else if token.kind != TokenKind::Comma && token.kind != TokenKind::NewLine {
                panic!("unexpected token after arg: {token:?}")
            }
        }

        (args, kwargs)
    }

    fn consume_whitespace(&mut self) {
        loop {
            match self.peek_kind() {
                None => break,
                Some(TokenKind::NewLine) => {}
                Some(_) => break,
            }

            self.iter.next();
        }
    }

    fn expect(&mut self, token: TokenKind) {
        let p = self.iter.next().expect("token");
        assert_eq!(p.kind, token, "Expected {token:?}, found {p:?}");
    }

    fn expect_ident(&mut self) -> Arc<str> {
        let p = self.iter.next().expect("token");
        let TokenKind::Identifier(name) = p.kind else {
            panic!("Expected identifier");
        };
        name
    }

    fn next_if_kind(&mut self, kind: &TokenKind) -> Option<Token> {
        self.iter.next_if(|t| t.kind == *kind)
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.iter.peek().map(|t| &t.kind)
    }
}

pub fn parse(tokens: Vec<Token>) -> Vec<AstNode> {
    Parser::new(tokens).parse()
}

fn parse_string(src: Arc<str>) -> Expression {
    let parts = interp::tokenise_string(src.as_ref());

    if parts.is_empty() {
        return Expression::String("".into()); // Use constant?
    }

    let res = parts
        .iter()
        .map(|p| match *p {
            StringToken::Str(s) => Expression::String(s.into()),
            StringToken::Expr(s) => {
                let tokens = lexer::tokenize(s).expect("TODO");
                Parser::new(tokens).parse_expression(0)
            }
        })
        .collect();

    Expression::StringInterpolate(res)
}
