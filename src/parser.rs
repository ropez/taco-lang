use std::{
    cmp::{self},
    sync::Arc,
    vec::IntoIter,
};

use multipeek::{IteratorExt, MultiPeek};

use crate::{
    error::{Error, Result},
    ident::Ident,
    interp::{self, StringTokenKind},
    lexer::{self, Loc, Src, Token, TokenKind},
};

#[derive(Debug)]
pub enum AstNode {
    Assignment {
        assignee: Src<Assignee>,
        value: Src<Expression>,
    },

    // Are functions expressions?
    // Is this just an assignment?
    Function {
        name: Ident,
        fun: Arc<Function>,
    },

    Iteration {
        ident: Ident,
        iterable: Src<Expression>,
        body: Vec<AstNode>,
    },

    Condition {
        cond: Src<Expression>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
    },

    Rec(Arc<Record>),
    Enum(Arc<Enumeration>),

    Expression(Src<Expression>),
    Return(Src<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Ref(Ident),
    PrefixedName(Ident, Ident),
    String(Arc<str>),
    StringInterpolate(Vec<(Src<Expression>, usize)>),
    Number(i64),
    True,
    False,
    Arguments,
    List(Vec<Src<Expression>>),
    Tuple(Arguments), // Should probably be (Option(name), expr)
    Not(Box<Src<Expression>>),
    Equal(Box<Src<Expression>>, Box<Src<Expression>>),
    NotEqual(Box<Src<Expression>>, Box<Src<Expression>>),
    Range(Box<Src<Expression>>, Box<Src<Expression>>),
    Addition(Box<Src<Expression>>, Box<Src<Expression>>),
    Subtraction(Box<Src<Expression>>, Box<Src<Expression>>),
    Multiplication(Box<Src<Expression>>, Box<Src<Expression>>),
    Division(Box<Src<Expression>>, Box<Src<Expression>>),
    Call {
        subject: Box<Src<Expression>>,
        arguments: Box<ArgumentsKind>,
    },
    Access {
        subject: Box<Src<Expression>>,
        key: Ident,
    },
}

#[derive(Debug)]
pub struct Argument {
    pub(crate) name: Option<Ident>,
    pub(crate) expr: Src<Expression>,
}

impl Argument {
    pub fn new(name: Option<Ident>, expr: Src<Expression>) -> Self {
        Self { name, expr }
    }

    pub fn named(name: Ident, expr: Src<Expression>) -> Self {
        Self::new(Some(name), expr)
    }

    pub fn unnamed(expr: Src<Expression>) -> Self {
        Self::new(None, expr)
    }
}

#[derive(Debug)]
pub struct Arguments {
    pub(crate) args: Vec<Argument>,
    pub(crate) loc: Loc,
}

#[derive(Debug)]
pub enum ArgumentsKind {
    Inline(Arguments),
    Destructure(Src<Expression>),
    DestructureImplicit(Loc),
}

#[derive(Debug)]
pub enum TypeExpression {
    Scalar(Ident),
    List(Box<Src<TypeExpression>>),
    Tuple(Vec<Parameter>),
}

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Vec<Parameter>,
    pub(crate) type_expr: Option<Src<TypeExpression>>,
    pub(crate) body: Vec<AstNode>,
}

#[derive(Debug)]
pub struct Record {
    pub(crate) name: Ident,
    pub(crate) params: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Parameter {
    pub(crate) name: Option<Ident>,
    pub(crate) type_expr: Src<TypeExpression>,
}

#[derive(Debug)]
pub struct Enumeration {
    pub(crate) name: Ident,
    pub(crate) variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub(crate) name: Ident,
    pub(crate) params: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Assignee {
    pub(crate) name: Option<Ident>,
    pub(crate) pattern: Option<Vec<Src<Assignee>>>,
}

impl Assignee {
    fn scalar(name: Ident) -> Self {
        Self {
            name: Some(name),
            pattern: None,
        }
    }

    fn destructure(name: Option<Ident>, pattern: Vec<Src<Assignee>>) -> Self {
        Self {
            name,
            pattern: Some(pattern),
        }
    }
}

pub struct Parser<'a> {
    src: &'a str,
    iter: MultiPeek<IntoIter<Token>>,
}

mod constants {
    pub(crate) const BP_EQUAL: u32 = 1;
    pub(crate) const BP_SPREAD: u32 = 6;
    pub(crate) const BP_PLUS: u32 = 10;
    pub(crate) const BP_MINUS: u32 = 10;
    pub(crate) const BP_DIV: u32 = 20;
    pub(crate) const BP_MULT: u32 = 20;
    pub(crate) const BP_CALL: u32 = 90;
    pub(crate) const BP_ACCESS: u32 = 100;
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            src,
            iter: tokens.into_iter().multipeek(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>> {
        self.parse_block(true)
    }

    pub fn parse_block(&mut self, root: bool) -> Result<Vec<AstNode>> {
        let mut ast = Vec::new();

        loop {
            let Some(token) = self.iter.next() else { break };

            match token.as_ref() {
                TokenKind::NewLine => {} // Ignore
                TokenKind::Comment(_) => {}
                TokenKind::RightBrace if !root => break,
                TokenKind::Fun => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let params = self.parse_params(TokenKind::RightParen)?;
                    let r = self.expect_kind(TokenKind::RightParen)?;

                    let type_expr = if self.next_if_kind(&TokenKind::Colon).is_some() {
                        Some(self.parse_type_expr()?)
                    } else {
                        None
                    };

                    self.expect_kind(TokenKind::LeftBrace)?;

                    let body = self.parse_block(false)?;

                    let fun = Arc::new(Function {
                        body,
                        params,
                        type_expr,
                    });
                    ast.push(AstNode::Function { name, fun });
                }
                TokenKind::Return => {
                    let expr = self.parse_expression(0)?;
                    ast.push(AstNode::Return(expr));
                }
                TokenKind::Identifier(name) => {
                    if self.next_if_kind(&TokenKind::Assign).is_some() {
                        let assignee = Assignee::scalar(name.clone());
                        let assignee = Src::new(assignee, token.loc);
                        let value = self.parse_expression(0)?;
                        ast.push(AstNode::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let expr = Src::new(Expression::Ref(name.clone()), token.loc);
                        let expr = self.parse_continuation(expr, 0)?;
                        ast.push(AstNode::Expression(expr));
                    }
                }
                TokenKind::String(s) => {
                    let expr = parse_string(s.as_ref(), token.loc)?;
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(AstNode::Expression(expr));
                }
                TokenKind::LeftSquare => {
                    let list = self.parse_expressions(TokenKind::RightSquare)?;
                    let end = self.expect_kind(TokenKind::RightSquare)?;
                    let loc = wrap_locations(token.loc, end.loc);

                    let expr = Src::new(Expression::List(list), loc);
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(AstNode::Expression(expr))
                }
                TokenKind::LeftParen => {
                    if let Some(TokenKind::Assign) = self.peek_after_paren() {
                        // XXX FIXME: Use peek in the outer loop, so that we don't need so much
                        // awkward juggling with locations and starting lists inside prackets
                        let assignee = self.parse_destructuring_pattern(None, &token)?;
                        self.expect_kind(TokenKind::Assign)?;
                        let value = self.parse_expression(0)?;
                        ast.push(AstNode::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let args = self.parse_args(token.loc)?;
                        let loc = args.loc;
                        let expr = Src::new(Expression::Tuple(args), loc);
                        let expr = self.parse_continuation(expr, 0)?;
                        ast.push(AstNode::Expression(expr));
                    }
                }
                TokenKind::If => {
                    let cond = self.parse_expression(0)?;
                    self.expect_kind(TokenKind::LeftBrace)?;
                    let body = self.parse_block(false)?;

                    let else_body = self
                        .next_if_kind(&TokenKind::Else)
                        .map(|_| {
                            self.expect_kind(TokenKind::LeftBrace)?;
                            self.parse_block(false)
                        })
                        .transpose()?;

                    ast.push(AstNode::Condition {
                        cond,
                        body,
                        else_body,
                    });
                }
                TokenKind::For => {
                    let (ident, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::In)?;
                    let iterable = self.parse_expression(0)?;
                    self.expect_kind(TokenKind::LeftBrace)?;
                    let body = self.parse_block(false)?;

                    ast.push(AstNode::Iteration {
                        ident,
                        iterable,
                        body,
                    });
                }
                TokenKind::Rec => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let params = self.parse_params(TokenKind::RightParen)?;
                    let r = self.expect_kind(TokenKind::RightParen)?;

                    let rec = Arc::new(Record { name, params });
                    ast.push(AstNode::Rec(rec));
                }
                TokenKind::Enum => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let variants = self.parse_variants()?;
                    self.expect_kind(TokenKind::RightParen)?;

                    let rec = Arc::new(Enumeration { name, variants });
                    ast.push(AstNode::Enum(rec));
                }
                _ => {
                    return Err(self.fail_at("Unexpected token", &token));
                }
            }
        }

        Ok(ast)
    }

    fn parse_expression(&mut self, bp: u32) -> Result<Src<Expression>> {
        let token = self.expect_token()?;

        let expr = match token.as_ref() {
            TokenKind::Identifier(s) => self.handle_identifier_expr(s.clone(), token.loc, bp)?,
            TokenKind::Not => {
                let expr = self.parse_expression(bp)?;
                let loc = wrap_locations(token.loc, expr.loc);
                Src::new(Expression::Not(expr.into()), loc)
            }
            TokenKind::String(s) => {
                let expr = parse_string(s, token.loc)?;
                self.parse_continuation(expr, bp)?
            }
            TokenKind::Number(n) => {
                let e = Src::new(Expression::Number(*n), token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::True => {
                let e = Src::new(Expression::True, token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::False => {
                let e = Src::new(Expression::False, token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::Arguments => {
                let e = Src::new(Expression::Arguments, token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::LeftSquare => {
                let list = self.parse_expressions(TokenKind::RightSquare)?;
                let end = self.expect_kind(TokenKind::RightSquare)?;
                let loc = wrap_locations(token.loc, end.loc);

                let expr = Src::new(Expression::List(list), loc);
                self.parse_continuation(expr, 0)?
            }
            TokenKind::LeftParen => {
                let list = self.parse_args(token.loc)?;
                let loc = list.loc;
                let expr = Src::new(Expression::Tuple(list), loc);
                self.parse_continuation(expr, 0)?
            }
            _ => return Err(self.fail_at("unexpected token", &token)),
        };

        Ok(expr)
    }

    fn handle_identifier_expr(
        &mut self,
        ident: Ident,
        loc: Loc,
        bp: u32,
    ) -> Result<Src<Expression>> {
        if ident.as_str() == "_" {
            return Err(self.fail("Expected identifier", loc));
        }
        if self.next_if_kind(&TokenKind::DoubleColon).is_some() {
            let (name, l) = self.expect_ident()?;
            let loc = wrap_locations(loc, l);
            let expr = Src::new(Expression::PrefixedName(ident, name), loc);
            self.parse_continuation(expr, bp)
        } else {
            let expr = Src::new(Expression::Ref(ident), loc);
            self.parse_continuation(expr, bp)
        }
    }

    fn parse_continuation(&mut self, lhs: Src<Expression>, bp: u32) -> Result<Src<Expression>> {
        use constants::*;

        let expr = match self.peek_kind() {
            None => lhs,
            Some(token) => match token {
                TokenKind::Comment(_) => lhs,
                TokenKind::Equal => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Equal(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::NotEqual => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::NotEqual(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Dot => {
                    if bp >= BP_ACCESS {
                        lhs
                    } else {
                        self.iter.next();
                        let (key, loc) = self.expect_ident()?;
                        let loc = wrap_locations(lhs.loc, loc);
                        let expr = Src::new(
                            Expression::Access {
                                subject: lhs.into(),
                                key,
                            },
                            loc,
                        );
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Spread => {
                    if bp >= BP_SPREAD {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_SPREAD)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Range(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Plus => {
                    if bp >= BP_PLUS {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_PLUS)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Addition(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Minus => {
                    if bp >= BP_MINUS {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_MINUS)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Subtraction(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Multiply => {
                    if bp >= BP_MULT {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_MULT)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr =
                            Src::new(Expression::Multiplication(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Divide => {
                    if bp >= BP_DIV {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_DIV)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Division(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::LeftParen => {
                    if bp >= BP_CALL {
                        lhs
                    } else {
                        let t = self.expect_token()?;

                        if let Some(a) = self.next_if_kind(&TokenKind::Assign) {
                            if let Some(t) = self.next_if_kind(&TokenKind::RightParen) {
                                let loc = wrap_locations(lhs.loc, t.loc);
                                let expr = Src::new(
                                    Expression::Call {
                                        subject: lhs.into(),
                                        arguments: ArgumentsKind::DestructureImplicit(a.loc).into(),
                                    },
                                    loc,
                                );
                                self.parse_continuation(expr, bp)?
                            } else {
                                let expr = self.parse_expression(0)?;
                                let t = self.expect_kind(TokenKind::RightParen)?;
                                let loc = wrap_locations(lhs.loc, t.loc);
                                let expr = Src::new(
                                    Expression::Call {
                                        subject: lhs.into(),
                                        arguments: ArgumentsKind::Destructure(expr).into(),
                                    },
                                    loc,
                                );
                                self.parse_continuation(expr, bp)?
                            }
                        } else {
                            let arguments = self.parse_args(t.loc)?;

                            let loc = wrap_locations(lhs.loc, arguments.loc);

                            let expr = Src::new(
                                Expression::Call {
                                    subject: lhs.into(),
                                    arguments: ArgumentsKind::Inline(arguments).into(),
                                },
                                loc,
                            );

                            self.parse_continuation(expr, bp)?
                        }
                    }
                }
                _ => lhs,
            },
        };

        Ok(expr)
    }

    fn parse_params(&mut self, until: TokenKind) -> Result<Vec<Parameter>> {
        let mut params = Vec::new();

        loop {
            self.consume_whitespace();
            let Some(next) = self.iter.peek() else {
                return Err(self.fail_at_end("Unexpected end of input"));
            };
            if *next.as_ref() == until {
                break;
            }

            if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
                let name = name.clone();
                let t = self.expect_token()?;

                if self.next_if_kind(&TokenKind::Colon).is_some() {
                    let type_expr = self.parse_type_expr()?;
                    params.push(Parameter {
                        name: Some(name),
                        type_expr,
                    });
                } else {
                    let type_expr = Src::new(TypeExpression::Scalar(name), t.loc);

                    params.push(Parameter {
                        name: None,
                        type_expr,
                    });
                }
            } else {
                let type_expr = self.parse_type_expr()?;
                params.push(Parameter {
                    name: None,
                    type_expr,
                });
            }

            let kind = self.peek_kind();
            match kind {
                Some(kind) if *kind == until => break,
                Some(TokenKind::Comma | TokenKind::NewLine) => {
                    self.iter.next();
                    continue;
                }
                _ => {
                    let token = self.expect_token()?;
                    return Err(self.fail_at("Unexpected token", &token));
                }
            }
        }

        Ok(params)
    }

    fn parse_expressions(&mut self, until: TokenKind) -> Result<Vec<Src<Expression>>> {
        self.parse_list(until, |p| p.parse_expression(0))
    }

    fn parse_destructuring_pattern(
        &mut self,
        name: Option<Ident>,
        token: &Token,
    ) -> Result<Src<Assignee>> {
        let pattern = self.parse_list(TokenKind::RightParen, |p| {
            if let Some(TokenKind::LeftParen) = p.peek_kind() {
                let t = p.expect_token()?;
                let assignee = p.parse_destructuring_pattern(None, &t)?;
                Ok(assignee)
            } else {
                let (ident, loc) = p.expect_ident()?;
                if p.next_if_kind(&TokenKind::Colon).is_some() {
                    let t = p.expect_kind(TokenKind::LeftParen)?;
                    let assignee = p.parse_destructuring_pattern(Some(ident), &t)?;
                    Ok(assignee)
                } else {
                    let assignee = Assignee::scalar(ident); // Discard if '_'
                    let assignee = Src::new(assignee, loc);
                    Ok(assignee)
                }
            }
        })?;

        let t = self.expect_kind(TokenKind::RightParen)?;
        let loc = wrap_locations(token.loc, t.loc);
        let assignee = Assignee::destructure(name, pattern);
        Ok(Src::new(assignee, loc))
    }

    fn parse_variants(&mut self) -> Result<Vec<Variant>> {
        self.parse_list(TokenKind::RightParen, |p| {
            let (name, _) = p.expect_ident()?;

            if p.next_if_kind(&TokenKind::LeftParen).is_some() {
                let params = p.parse_params(TokenKind::RightParen)?;
                let r = p.expect_kind(TokenKind::RightParen)?;

                Ok(Variant { name, params })
            } else {
                Ok(Variant {
                    name,
                    params: Vec::new(),
                })
            }
        })
    }

    fn parse_args(&mut self, start_loc: Loc) -> Result<Arguments> {
        let mut args = Vec::new();

        loop {
            self.consume_whitespace();
            if let Some(TokenKind::RightParen) = self.peek_kind() {
                break;
            }

            if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
                let name = name.clone();
                let t = self.expect_token()?;

                if self.next_if_kind(&TokenKind::Colon).is_some() {
                    let value = self.parse_expression(0)?;
                    args.push(Argument::named(name.clone(), value));
                } else {
                    let expr = self.handle_identifier_expr(name.clone(), t.loc, 0)?;

                    args.push(Argument::unnamed(expr));
                }
            } else {
                let expr = self.parse_expression(0)?;
                args.push(Argument::unnamed(expr));
            }

            let kind = self.peek_kind();
            match kind {
                Some(TokenKind::RightParen) => break,
                Some(TokenKind::Comma | TokenKind::NewLine) => {
                    self.iter.next();
                    continue;
                }
                _ => {
                    let token = self.expect_token()?;
                    return Err(self.fail_at("Unexpected token", &token));
                }
            }
        }

        let end = self.expect_kind(TokenKind::RightParen)?;
        let loc = wrap_locations(start_loc, end.loc);
        Ok(Arguments { args, loc })
    }

    fn parse_type_expr(&mut self) -> Result<Src<TypeExpression>> {
        if let Some(l) = self.next_if_kind(&TokenKind::LeftSquare) {
            let inner = self.parse_type_expr()?;
            let r = self.expect_kind(TokenKind::RightSquare)?;

            let kind = TypeExpression::List(inner.into());
            Ok(Src::new(kind, wrap_locations(l.loc, r.loc)))
        } else if let Some(l) = self.next_if_kind(&TokenKind::LeftParen) {
            let params = self.parse_params(TokenKind::RightParen)?;
            let r = self.expect_kind(TokenKind::RightParen)?;

            let kind = TypeExpression::Tuple(params);
            Ok(Src::new(kind, wrap_locations(l.loc, r.loc)))
        } else {
            let (raw, loc) = self.expect_ident()?;
            let kind = TypeExpression::Scalar(raw);
            Ok(Src::new(kind, loc))
        }
    }

    fn parse_list<T, P>(&mut self, until: TokenKind, mut item_parser: P) -> Result<Vec<T>>
    where
        P: FnMut(&mut Self) -> Result<T>,
    {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let Some(next) = self.iter.peek() else {
                return Err(self.fail_at_end("Unexpected end of input"));
            };
            if *next.as_ref() == until {
                break;
            }

            items.push(item_parser(self)?);

            let kind = self.peek_kind();
            match kind {
                Some(kind) if *kind == until => break,
                Some(TokenKind::Comma | TokenKind::NewLine) => {
                    self.iter.next();
                    continue;
                }
                _ => {
                    let token = self.expect_token()?;
                    return Err(self.fail_at("Unexpected token", &token));
                }
            }
        }

        Ok(items)
    }

    fn consume_whitespace(&mut self) {
        loop {
            match self.peek_kind() {
                None => break,
                Some(TokenKind::NewLine) => {}
                Some(TokenKind::Comment(_)) => {}
                Some(_) => break,
            }

            self.iter.next();
        }
    }

    fn expect_end_of_line(&mut self) -> Result<()> {
        loop {
            match self.iter.next() {
                None => break,
                Some(token) => match token.as_ref() {
                    TokenKind::NewLine => break,
                    TokenKind::Comment(_) => {}
                    _ => return Err(self.fail_at("Unexpected token", &token)),
                },
            }
        }

        Ok(())
    }

    fn expect_token(&mut self) -> Result<Token> {
        self.iter
            .next()
            .ok_or_else(|| self.fail_at_end("Unexpected end of input"))
    }

    fn expect_kind(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.expect_token()?;
        if *token.as_ref() == kind {
            Ok(token)
        } else {
            Err(self.fail_at(&format!("Expected to find {kind:?} here"), &token))
        }
    }

    fn expect_ident(&mut self) -> Result<(Ident, Loc)> {
        let token = self.expect_token()?;
        match token.as_ref() {
            TokenKind::Identifier(name) => Ok((name.clone(), token.loc)),
            _ => Err(self.fail_at("Expected identifier", &token)),
        }
    }

    fn next_if_kind(&mut self, kind: &TokenKind) -> Option<Token> {
        if self.iter.peek().filter(|t| *t.as_ref() == *kind).is_some() {
            self.iter.next()
        } else {
            None
        }
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.iter.peek().map(|t| t.as_ref())
    }

    fn peek_after_paren(&mut self) -> Option<&TokenKind> {
        let mut d = 1;
        for n in 0.. {
            let token = self.iter.peek_nth(n)?;

            match token.as_ref() {
                TokenKind::LeftParen => d += 1,
                TokenKind::RightParen => {
                    d -= 1;
                    if d == 0 {
                        return self.iter.peek_nth(n + 1).map(|t| t.as_ref());
                    }
                }
                _ => {}
            }
        }

        None
    }

    fn fail_at(&self, msg: &str, token: &Token) -> Error {
        self.fail(msg, token.loc)
    }

    fn fail_at_end(&self, msg: &str) -> Error {
        let len = self.src.len();
        self.fail(msg, Loc::new(len - 1, len))
    }

    fn fail(&self, msg: &str, loc: Loc) -> Error {
        use std::backtrace::Backtrace;
        println!("{}", Backtrace::force_capture());
        Error::new(msg.into(), self.src, loc)
    }
}

fn parse_string(src: &str, loc: Loc) -> Result<Src<Expression>> {
    let parts = interp::tokenise_string(src);

    if parts.is_empty() {
        return Ok(Src::new(Expression::String("".into()), loc));
    }

    let mut res = Vec::new();

    let start_offset = loc.start + 1;
    for part in parts {
        let expr = match &part.kind {
            StringTokenKind::Str => {
                Src::new(Expression::String(part.src.into()), Loc::new(0, part.src.len()))
            }
            StringTokenKind::Expr => {
                let tokens = lexer::tokenize(part.src)?;
                Parser::new(part.src, tokens).parse_expression(0)?
            }
        };

        res.push((expr, start_offset + part.offset));
    }

    Ok(Src::new(Expression::StringInterpolate(res), loc))
}

fn wrap_locations(start: Loc, end: Loc) -> Loc {
    Loc::new(cmp::min(start.start, end.start), cmp::max(start.end, end.end))
}
