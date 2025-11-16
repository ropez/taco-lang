use std::{
    cmp::{self},
    iter::Peekable,
    ops::Range,
    sync::Arc,
    vec::IntoIter,
};

use crate::{
    error::{Error, Result},
    interp::{self, StringToken},
    lexer::{self, Token, TokenKind},
};

#[derive(Debug)]
pub enum AstNode {
    Assignment {
        assignee: Assignmee,
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

    Rec(Arc<Record>),
    Enum(Arc<Enumeration>),

    Expression(Expression),
    Return(Expression),
}

#[derive(Debug)]
pub struct Expression {
    pub(crate) kind: ExpressionKind,
    pub(crate) loc: Range<usize>,
}

impl Expression {
    fn new(kind: ExpressionKind, loc: Range<usize>) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug)]
pub(crate) enum ExpressionKind {
    Ref(Arc<str>),
    PrefixedName(Arc<str>, Arc<str>),
    String(Arc<str>),
    StringInterpolate(Vec<Expression>),
    Number(i64),
    True,
    False,
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Not(Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    Range(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Call {
        subject: Box<Expression>,
        arguments: Arguments,
    },
    Access {
        subject: Box<Expression>,
        key: Arc<str>,
    },
}

#[derive(Debug)]
pub struct Arguments {
    pub(crate) args: Vec<Expression>,
    pub(crate) kwargs: Vec<(Arc<str>, Expression)>,
}

#[derive(Debug)]
pub struct TypeExpression {
    pub(crate) kind: TypeExpressionKind,
    pub(crate) loc: Range<usize>,
}

#[derive(Debug)]
pub enum TypeExpressionKind {
    Scalar(Arc<str>),
    List(Box<TypeExpression>),
    Tuple(Vec<TypeExpression>),
}

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Vec<Parameter>,
    pub(crate) type_expr: Option<TypeExpression>,
    pub(crate) body: Vec<AstNode>,
}

#[derive(Debug)]
pub struct Record {
    pub(crate) name: Arc<str>,
    pub(crate) params: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Parameter {
    pub(crate) name: Arc<str>, // Can also be destructuring
    pub(crate) type_expr: TypeExpression,
}

#[derive(Debug)]
pub struct Enumeration {
    pub(crate) name: Arc<str>,
    pub(crate) variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub(crate) name: Arc<str>,
    pub(crate) type_exprs: Vec<TypeExpression>,
}

#[derive(Debug)]
pub enum Assignmee {
    Scalar(Arc<str>),
    Destructure(Vec<Arc<str>>),
}

pub struct Parser<'a> {
    src: &'a str,
    iter: Peekable<IntoIter<Token>>,
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
            iter: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>> {
        self.parse_block(true)
    }

    pub fn parse_block(&mut self, root: bool) -> Result<Vec<AstNode>> {
        let mut ast = Vec::new();

        loop {
            let Some(token) = self.iter.next() else { break };

            match token.kind {
                TokenKind::NewLine => {} // Ignore
                TokenKind::RightBrace if !root => break,
                TokenKind::Fun => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let params = self.parse_params(TokenKind::RightParen)?;

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
                        let assignee = Assignmee::Scalar(name);
                        let value = self.parse_expression(0)?;
                        ast.push(AstNode::Assignment { assignee, value });
                        self.expect_kind(TokenKind::NewLine)?;
                    } else {
                        let expr = Expression::new(ExpressionKind::Ref(name), token.loc);
                        let expr = self.parse_continuation(expr, 0)?;
                        ast.push(AstNode::Expression(expr));
                    }
                }
                TokenKind::String(s) => {
                    let expr = parse_string(s, token.loc)?;
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(AstNode::Expression(expr));
                }
                TokenKind::LeftSquare => {
                    let list = self.parse_expressions(TokenKind::RightSquare)?;
                    let end = self.expect_kind(TokenKind::RightSquare)?;
                    let loc = wrap_locations(&token.loc, &end.loc);

                    let expr = Expression::new(ExpressionKind::List(list), loc);
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(AstNode::Expression(expr))
                }
                TokenKind::LeftParen => {
                    if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
                        let idents = self.parse_ident_list()?;
                        self.expect_kind(TokenKind::RightParen)?;
                        self.expect_kind(TokenKind::Assign)?; // XXX Can also be a tuple expression
                        let value = self.parse_expression(0)?;
                        let assignee = Assignmee::Destructure(idents);
                        ast.push(AstNode::Assignment { assignee, value });
                        self.expect_kind(TokenKind::NewLine)?;
                    } else {
                        let list = self.parse_expressions(TokenKind::RightParen)?;
                        let end = self.expect_kind(TokenKind::RightParen)?;
                        let loc = wrap_locations(&token.loc, &end.loc);

                        let expr = Expression::new(ExpressionKind::Tuple(list), loc);
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
                    let (ident, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let params = self.parse_params(TokenKind::RightParen)?;

                    let rec = Arc::new(Record {
                        name: ident,
                        params,
                    });
                    ast.push(AstNode::Rec(rec));
                }
                TokenKind::Enum => {
                    let (ident, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let variants = self.parse_variants()?;
                    self.expect_kind(TokenKind::RightParen)?;

                    let rec = Arc::new(Enumeration {
                        name: ident,
                        variants,
                    });
                    ast.push(AstNode::Enum(rec));
                }
                _ => {
                    return Err(self.fail_at("Unexpected token", &token));
                }
            }
        }

        Ok(ast)
    }

    fn parse_expression(&mut self, bp: u32) -> Result<Expression> {
        let token = self.expect_token()?;

        let expr = match token.kind {
            TokenKind::Identifier(s) => self.handle_identifier_expr(s, token.loc, bp)?,
            TokenKind::Not => {
                let expr = self.parse_expression(bp)?;
                let loc = wrap_locations(&token.loc, &expr.loc);
                Expression::new(ExpressionKind::Not(expr.into()), loc)
            }
            TokenKind::String(s) => {
                let expr = parse_string(s, token.loc)?;
                self.parse_continuation(expr, bp)?
            }
            TokenKind::Number(n) => {
                let e = Expression::new(ExpressionKind::Number(n), token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::True => {
                let e = Expression::new(ExpressionKind::True, token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::False => {
                let e = Expression::new(ExpressionKind::False, token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::LeftSquare => {
                let list = self.parse_expressions(TokenKind::RightSquare)?;
                let end = self.expect_kind(TokenKind::RightSquare)?;
                let loc = wrap_locations(&token.loc, &end.loc);

                let expr = Expression::new(ExpressionKind::List(list), loc);
                self.parse_continuation(expr, 0)?
            }
            TokenKind::LeftParen => {
                let list = self.parse_expressions(TokenKind::RightParen)?;
                let end = self.expect_kind(TokenKind::RightParen)?;
                let loc = wrap_locations(&token.loc, &end.loc);

                let expr = Expression::new(ExpressionKind::Tuple(list), loc);
                self.parse_continuation(expr, 0)?
            }
            _ => return Err(self.fail_at("unexpected token", &token)),
        };

        Ok(expr)
    }

    fn handle_identifier_expr(
        &mut self,
        s: Arc<str>,
        loc: Range<usize>,
        bp: u32,
    ) -> Result<Expression> {
        if s.as_ref() == "_" {
            return Err(self.fail("Expected identifier", &loc));
        }
        if self.next_if_kind(&TokenKind::DoubleColon).is_some() {
            let (name, l) = self.expect_ident()?;
            let loc = wrap_locations(&loc, &l);
            let expr = Expression::new(ExpressionKind::PrefixedName(s, name), loc);
            self.parse_continuation(expr, bp)
        } else {
            let expr = Expression::new(ExpressionKind::Ref(s), loc);
            self.parse_continuation(expr, bp)
        }
    }

    fn parse_continuation(&mut self, lhs: Expression, bp: u32) -> Result<Expression> {
        use constants::*;

        let expr = match self.peek_kind() {
            None => lhs,
            Some(token) => match token {
                TokenKind::Equal => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr =
                            Expression::new(ExpressionKind::Equal(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::NotEqual => {
                    if bp >= BP_EQUAL {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_EQUAL)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr =
                            Expression::new(ExpressionKind::NotEqual(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Dot => {
                    if bp >= BP_ACCESS {
                        lhs
                    } else {
                        self.iter.next();
                        let (key, loc) = self.expect_ident()?;
                        let loc = wrap_locations(&lhs.loc, &loc);
                        let expr = Expression::new(
                            ExpressionKind::Access {
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
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr =
                            Expression::new(ExpressionKind::Range(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Plus => {
                    if bp >= BP_PLUS {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_PLUS)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr =
                            Expression::new(ExpressionKind::Addition(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Minus => {
                    if bp >= BP_MINUS {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_MINUS)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr = Expression::new(
                            ExpressionKind::Subtraction(lhs.into(), rhs.into()),
                            loc,
                        );
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Multiply => {
                    if bp >= BP_MULT {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_MULT)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr = Expression::new(
                            ExpressionKind::Multiplication(lhs.into(), rhs.into()),
                            loc,
                        );
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::Divide => {
                    if bp >= BP_DIV {
                        lhs
                    } else {
                        self.iter.next();
                        let rhs = self.parse_expression(BP_DIV)?;
                        let loc = wrap_locations(&lhs.loc, &rhs.loc);
                        let expr =
                            Expression::new(ExpressionKind::Division(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                TokenKind::LeftParen => {
                    if bp >= BP_CALL {
                        lhs
                    } else {
                        self.iter.next();

                        let arguments = self.parse_args()?;

                        let t = self.expect_kind(TokenKind::RightParen)?;
                        let loc = wrap_locations(&lhs.loc, &t.loc);

                        let expr = Expression::new(
                            ExpressionKind::Call {
                                subject: lhs.into(),
                                arguments,
                            },
                            loc,
                        );

                        self.parse_continuation(expr, bp)?
                    }
                }
                _ => lhs,
            },
        };

        Ok(expr)
    }

    fn parse_params(&mut self, until: TokenKind) -> Result<Vec<Parameter>> {
        let mut items = Vec::new();

        loop {
            self.consume_whitespace();
            let Some(next) = self.iter.peek() else {
                return Err(self.fail_at_end("Unexpected end of input"));
            };
            if next.kind == until {
                self.iter.next();
                break;
            }

            let (name, _) = self.expect_ident()?;
            self.expect_kind(TokenKind::Colon)?;
            let type_expr = self.parse_type_expr()?;
            let param = Parameter { name, type_expr };
            items.push(param);

            let token = self.expect_token()?;
            if token.kind == until {
                break;
            } else if token.kind != TokenKind::Comma && token.kind != TokenKind::NewLine {
                return Err(self.fail_at("Unexpected token", &token));
            }
        }

        Ok(items)
    }

    fn parse_expressions(&mut self, until: TokenKind) -> Result<Vec<Expression>> {
        self.parse_list(until, |p| p.parse_expression(0))
    }

    fn parse_ident_list(&mut self) -> Result<Vec<Arc<str>>> {
        self.parse_list(TokenKind::RightParen, |p| {
            let (ident, _) = p.expect_ident()?;
            Ok(ident)
        })
    }

    fn parse_variants(&mut self) -> Result<Vec<Variant>> {
        self.parse_list(TokenKind::RightParen, |p| {
            let (name, _) = p.expect_ident()?;

            if p.next_if_kind(&TokenKind::LeftParen).is_some() {
                let params = p.parse_list(TokenKind::RightParen, |p| p.parse_type_expr())?;
                let r = p.expect_kind(TokenKind::RightParen)?;

                Ok(Variant {
                    name,
                    type_exprs: params,
                })
            } else {
                Ok(Variant {
                    name,
                    type_exprs: Default::default(),
                })
            }
        })
    }

    fn parse_args(&mut self) -> Result<Arguments> {
        let mut args = Vec::new();
        let mut kwargs = Vec::new();

        loop {
            self.consume_whitespace();
            if let Some(TokenKind::RightParen) = self.peek_kind() {
                break;
            }

            if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
                let name = Arc::clone(name);
                let t = self.expect_token()?;

                if self.next_if_kind(&TokenKind::Colon).is_some() {
                    let value = self.parse_expression(0)?;
                    kwargs.push((name, value));
                } else {
                    let expr = self.handle_identifier_expr(name, t.loc, 0)?;

                    if !kwargs.is_empty() {
                        return Err(self.fail(
                            "Unexpected positional argument after keyword argument",
                            &expr.loc,
                        ));
                    }

                    args.push(expr);
                }
            } else {
                let expr = self.parse_expression(0)?;
                if !kwargs.is_empty() {
                    return Err(self.fail(
                        "Unexpected positional argument after keyword argument",
                        &expr.loc,
                    ));
                }
                args.push(expr);
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

        Ok(Arguments { args, kwargs })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpression> {
        if let Some(l) = self.next_if_kind(&TokenKind::LeftSquare) {
            let inner = self.parse_type_expr()?;
            let r = self.expect_kind(TokenKind::RightSquare)?;

            let kind = TypeExpressionKind::List(inner.into());
            Ok(TypeExpression {
                kind,
                loc: wrap_locations(&l.loc, &r.loc),
            })
        } else if let Some(l) = self.next_if_kind(&TokenKind::LeftParen) {
            let types = self.parse_list(TokenKind::RightParen, |p| p.parse_type_expr())?;
            let r = self.expect_kind(TokenKind::RightParen)?;

            let kind = TypeExpressionKind::Tuple(types);
            Ok(TypeExpression {
                kind,
                loc: wrap_locations(&l.loc, &r.loc),
            })
        } else {
            let (raw, loc) = self.expect_ident()?;
            let kind = TypeExpressionKind::Scalar(raw);
            Ok(TypeExpression { kind, loc })
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
            if next.kind == until {
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
                Some(_) => break,
            }

            self.iter.next();
        }
    }

    fn expect_token(&mut self) -> Result<Token> {
        self.iter
            .next()
            .ok_or_else(|| self.fail_at_end("Unexpected end of input"))
    }

    fn expect_kind(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.expect_token()?;
        if token.kind == kind {
            Ok(token)
        } else {
            Err(self.fail_at(&format!("Expected to find {kind:?} here"), &token))
        }
    }

    fn expect_ident(&mut self) -> Result<(Arc<str>, Range<usize>)> {
        let token = self.expect_token()?;
        match token.kind {
            TokenKind::Identifier(name) => Ok((name, token.loc)),
            _ => Err(self.fail_at("Expected identifier", &token)),
        }
    }

    fn next_if_kind(&mut self, kind: &TokenKind) -> Option<Token> {
        self.iter.next_if(|t| t.kind == *kind)
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.iter.peek().map(|t| &t.kind)
    }

    fn fail_at(&self, msg: &str, token: &Token) -> Error {
        self.fail(msg, &token.loc)
    }

    fn fail_at_end(&self, msg: &str) -> Error {
        let len = self.src.len();
        self.fail(msg, &(len - 1..len))
    }

    fn fail(&self, msg: &str, loc: &Range<usize>) -> Error {
        use std::backtrace::Backtrace;
        println!("Custom backtrace: {}", Backtrace::force_capture());
        Error::new(msg.into(), self.src, loc)
    }
}

fn parse_string(src: Arc<str>, loc: Range<usize>) -> Result<Expression> {
    let parts = interp::tokenise_string(src.as_ref());

    // XXX Fix inner locations

    if parts.is_empty() {
        return Ok(Expression::new(ExpressionKind::String("".into()), loc));
    }

    let mut res = Vec::new();

    for part in parts {
        let expr = match part {
            StringToken::Str(s) => Expression::new(ExpressionKind::String(s.into()), loc.clone()),
            StringToken::Expr(s) => {
                let tokens = lexer::tokenize(s)?;
                Parser::new(s, tokens).parse_expression(0)?
            }
        };

        res.push(expr)
    }

    Ok(Expression::new(ExpressionKind::StringInterpolate(res), loc))
}

fn wrap_locations(start: &Range<usize>, end: &Range<usize>) -> Range<usize> {
    cmp::min(start.start, end.start)..cmp::max(start.end, end.end)
}
