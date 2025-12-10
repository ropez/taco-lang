use std::{
    cmp::{self},
    sync::Arc,
    vec::IntoIter,
};

use multipeek::{IteratorExt, MultiPeek};

use crate::{
    error::{Error, Result},
    ident::Ident,
    interpopation::{self, StringTokenKind},
    lexer::{self, Loc, Src, Token, TokenKind},
};

#[derive(Debug)]
pub enum Statement {
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
        body: Vec<Statement>,
    },

    Condition {
        cond: Src<Expression>,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },

    IfIn {
        assignee: Src<Assignee>,
        value: Src<Expression>,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },

    Rec(Arc<Record>),
    Enum(Arc<Enumeration>),

    Return(Option<Src<Expression>>),
    Assert(Src<Expression>),

    // Everything else is expressions
    Expression(Src<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Ref(Ident),
    PrefixedName(Ident, Ident),
    Str(Arc<str>),
    String(Vec<(Src<Expression>, usize)>),
    Int(i64),
    True,
    False,
    Arguments,
    List(Vec<Src<Expression>>),
    Tuple(Src<Vec<ArgumentExpression>>),
    Not(Box<Src<Expression>>),
    Equal(Box<Src<Expression>>, Box<Src<Expression>>),
    NotEqual(Box<Src<Expression>>, Box<Src<Expression>>),
    LessThan(Box<Src<Expression>>, Box<Src<Expression>>),
    GreaterThan(Box<Src<Expression>>, Box<Src<Expression>>),
    LessOrEqual(Box<Src<Expression>>, Box<Src<Expression>>),
    GreaterOrEqual(Box<Src<Expression>>, Box<Src<Expression>>),
    Range(Box<Src<Expression>>, Box<Src<Expression>>),
    Negate(Box<Src<Expression>>),
    Addition(Box<Src<Expression>>, Box<Src<Expression>>),
    Subtraction(Box<Src<Expression>>, Box<Src<Expression>>),
    Multiplication(Box<Src<Expression>>, Box<Src<Expression>>),
    Division(Box<Src<Expression>>, Box<Src<Expression>>),
    Modulo(Box<Src<Expression>>, Box<Src<Expression>>),

    // An expression followed by the '?' operator
    Try(Box<Src<Expression>>),
    Call {
        subject: Box<Src<Expression>>,
        arguments: Box<CallExpression>,
    },
    Access {
        subject: Box<Src<Expression>>,
        key: Ident,
    },

    Function(Arc<Function>),
}

#[derive(Debug)]
pub struct ArgumentExpression {
    pub(crate) name: Option<Ident>,
    pub(crate) expr: Src<Expression>, // Src outside?
}

impl ArgumentExpression {
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
pub enum CallExpression {
    Inline(Src<Vec<ArgumentExpression>>),
    Destructure(Src<Expression>),
    DestructureImplicit(Loc),
}

#[derive(Debug)]
pub enum TypeExpression {
    Scalar(Ident),
    List(Box<Src<TypeExpression>>),
    Opt(Box<Src<TypeExpression>>),
    Tuple(Vec<ParamExpression>),
}

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Vec<ParamExpression>,
    pub(crate) type_expr: Option<Src<TypeExpression>>,
    pub(crate) body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Record {
    pub(crate) name: Ident,
    pub(crate) params: Vec<ParamExpression>,
}

#[derive(Debug)]
pub struct ParamExpression {
    pub(crate) name: Option<Ident>,
    pub(crate) type_expr: Src<TypeExpression>,
    pub(crate) attrs: Vec<Src<AttributeExpression>>,
}

impl ParamExpression {
    pub(crate) fn is_optional(&self) -> bool {
        matches!(*self.type_expr, TypeExpression::Opt(_))
    }
}

#[derive(Debug)]
pub struct AttributeExpression {
    pub(crate) name: Ident,
    pub(crate) args: Option<Src<Vec<ArgumentExpression>>>,
}

#[derive(Debug)]
pub struct Enumeration {
    pub(crate) name: Ident,
    pub(crate) variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub(crate) name: Ident,
    pub(crate) params: Option<Vec<ParamExpression>>,
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
    pub(crate) const BP_QUESTION: u32 = 2;
    pub(crate) const BP_EQUAL: u32 = 3;
    pub(crate) const BP_CMP: u32 = 4;
    pub(crate) const BP_SPREAD: u32 = 6;
    pub(crate) const BP_PLUS: u32 = 10;
    pub(crate) const BP_MINUS: u32 = 10;
    pub(crate) const BP_DIV: u32 = 20;
    pub(crate) const BP_MULT: u32 = 20;
    pub(crate) const BP_CALL: u32 = 90;
    pub(crate) const BP_NEGATE: u32 = 95;
    pub(crate) const BP_ACCESS: u32 = 100;
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            src,
            iter: tokens.into_iter().multipeek(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        self.parse_block(true)
    }

    pub fn parse_single_expression(&mut self) -> Result<Src<Expression>> {
        let expr = self.parse_expression(0)?;

        if let Some(token) = self.iter.next() {
            return Err(self.fail("Unexpected token", token.loc));
        }

        Ok(expr)
    }

    pub fn parse_block(&mut self, root: bool) -> Result<Vec<Statement>> {
        let mut ast = Vec::new();

        while let Some(token) = self.iter.next() {
            match token.as_ref() {
                TokenKind::NewLine => {} // Ignore
                TokenKind::Comment(_) => {}
                TokenKind::RightBrace if !root => break,
                TokenKind::Fun => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftParen)?;
                    let params = self.parse_params(TokenKind::RightParen)?;
                    let r = self.expect_kind(TokenKind::RightParen)?;

                    // XXX Allow "implicit" type expression, and require it for implicit return:
                    // fun foo(): { 42 }

                    let type_expr = if self.next_if_kind(&TokenKind::Colon).is_some() {
                        Some(self.parse_type_expr()?)
                    } else {
                        None
                    };

                    self.expect_kind(TokenKind::LeftBrace)?;

                    let body = self.parse_block(false)?;
                    self.expect_end_of_line()?;

                    let fun = Arc::new(Function {
                        body,
                        params,
                        type_expr,
                    });
                    ast.push(Statement::Function { name, fun });
                }
                TokenKind::Return => {
                    if let Some(TokenKind::NewLine | TokenKind::RightBrace) = self.peek_kind() {
                        ast.push(Statement::Return(None));
                    } else {
                        let expr = self.parse_expression(0)?;
                        self.expect_end_of_line()?;
                        ast.push(Statement::Return(Some(expr)));
                    }
                }
                TokenKind::Assert => {
                    let expr = self.parse_expression(0)?;
                        self.expect_end_of_line()?;
                    ast.push(Statement::Assert(expr));
                }
                TokenKind::Identifier(name) => {
                    if self.next_if_kind(&TokenKind::Assign).is_some() {
                        let assignee = Assignee::scalar(name.clone());
                        let assignee = Src::new(assignee, token.loc);
                        let value = self.parse_expression(0)?;
                        ast.push(Statement::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let expr = Src::new(Expression::Ref(name.clone()), token.loc);
                        let expr = self.parse_continuation(expr, 0)?;
                        ast.push(Statement::Expression(expr));
                    }
                }
                TokenKind::Number(num) => {
                    let expr = Src::new(Expression::Int(*num), token.loc);
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(Statement::Expression(expr));
                }
                TokenKind::Minus => {
                    let expr = self.parse_expression(constants::BP_NEGATE)?;
                    let loc = wrap_locations(token.loc, expr.loc);
                    let expr = Expression::Negate(expr.into());
                    ast.push(Statement::Expression(Src::new(expr, loc)));
                }
                TokenKind::String(s) => {
                    let expr = self.parse_string(s.as_ref(), token.loc)?;
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(Statement::Expression(expr));
                }
                TokenKind::LeftBrace => {
                    let expr = self.parse_expression(0)?;
                    self.expect_kind(TokenKind::RightBrace)?;
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(Statement::Expression(expr));
                }
                TokenKind::LeftSquare => {
                    let list = self.parse_expressions(TokenKind::RightSquare)?;
                    let end = self.expect_kind(TokenKind::RightSquare)?;
                    let loc = wrap_locations(token.loc, end.loc);

                    let expr = Src::new(Expression::List(list), loc);
                    let expr = self.parse_continuation(expr, 0)?;
                    ast.push(Statement::Expression(expr))
                }
                TokenKind::LeftParen => {
                    if let Some(TokenKind::Assign) = self.peek_after_paren() {
                        // XXX FIXME: Use peek in the outer loop, so that we don't need so much
                        // awkward juggling with locations and starting lists inside prackets
                        let assignee = self.parse_destructuring_pattern(None, &token)?;
                        self.expect_kind(TokenKind::Assign)?;
                        let value = self.parse_expression(0)?;
                        ast.push(Statement::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let args = self.parse_args(token.loc)?;
                        let loc = args.loc;
                        let expr = Src::new(Expression::Tuple(args), loc);
                        let expr = self.parse_continuation(expr, 0)?;
                        ast.push(Statement::Expression(expr));
                    }
                }
                TokenKind::If => {
                    if self.peek_is_assignee_followed_by_in() {
                        let assignee = self.parse_assignee()?;
                        self.expect_kind(TokenKind::In)?;
                        let value = self.parse_expression(0)?;

                        self.expect_kind(TokenKind::LeftBrace)?;
                        let body = self.parse_block(false)?;

                        // XXX DRY
                        let else_body = self
                            .next_if_kind(&TokenKind::Else)
                            .map(|_| {
                                self.expect_kind(TokenKind::LeftBrace)?;
                                self.parse_block(false)
                            })
                            .transpose()?;

                        self.expect_end_of_line()?;

                        ast.push(Statement::IfIn {
                            assignee,
                            value,
                            body,
                            else_body,
                        });
                    } else {
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

                        ast.push(Statement::Condition {
                            cond,
                            body,
                            else_body,
                        });
                    }
                }
                TokenKind::For => {
                    let (ident, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::In)?;
                    let iterable = self.parse_expression(0)?;
                    self.expect_kind(TokenKind::LeftBrace)?;
                    let body = self.parse_block(false)?;

                    self.expect_end_of_line()?;

                    ast.push(Statement::Iteration {
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
                    self.expect_end_of_line()?;

                    let rec = Arc::new(Record { name, params });
                    ast.push(Statement::Rec(rec));
                }
                TokenKind::Enum => {
                    let (name, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::LeftBrace)?;
                    let variants = self.parse_variants()?;
                    self.expect_kind(TokenKind::RightBrace)?;
                    self.expect_end_of_line()?;

                    let rec = Arc::new(Enumeration { name, variants });
                    ast.push(Statement::Enum(rec));
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
                // FIX continuation: !foo || bar
                let expr = self.parse_expression(bp)?;
                let loc = wrap_locations(token.loc, expr.loc);
                Src::new(Expression::Not(expr.into()), loc)
            }
            TokenKind::String(s) => {
                let expr = self.parse_string(s, token.loc)?;
                self.parse_continuation(expr, bp)?
            }
            TokenKind::Number(n) => {
                let e = Src::new(Expression::Int(*n), token.loc);
                self.parse_continuation(e, bp)?
            }
            TokenKind::Minus => {
                let expr = self.parse_expression(constants::BP_NEGATE)?;
                let loc = wrap_locations(token.loc, expr.loc);
                let expr = Src::new(Expression::Negate(expr.into()), loc);
                self.parse_continuation(expr, bp)?
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
            TokenKind::Fun => {
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

                // XXX Need end location of entire block
                let loc = wrap_locations(token.loc, r.loc);
                let expr = Src::new(Expression::Function(fun), loc);
                self.parse_continuation(expr, bp)?
            }
            TokenKind::LeftBrace => {
                let expr = self.parse_expression(0)?;
                self.expect_kind(TokenKind::RightBrace)?;
                self.parse_continuation(expr, bp)?
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
                TokenKind::Equal => self.parse_binary_expr(lhs, Expression::Equal, BP_EQUAL, bp)?,
                TokenKind::NotEqual => {
                    self.parse_binary_expr(lhs, Expression::NotEqual, BP_EQUAL, bp)?
                }
                TokenKind::LessThan => {
                    self.parse_binary_expr(lhs, Expression::LessThan, BP_CMP, bp)?
                }
                TokenKind::GreaterThan => {
                    self.parse_binary_expr(lhs, Expression::GreaterThan, BP_CMP, bp)?
                }
                TokenKind::LessOrEqual => {
                    self.parse_binary_expr(lhs, Expression::LessOrEqual, BP_CMP, bp)?
                }
                TokenKind::GreaterOrEqual => {
                    self.parse_binary_expr(lhs, Expression::GreaterOrEqual, BP_CMP, bp)?
                }
                TokenKind::Plus => {
                    self.parse_binary_expr(lhs, Expression::Addition, BP_PLUS, bp)?
                }
                TokenKind::Minus => {
                    self.parse_binary_expr(lhs, Expression::Subtraction, BP_MINUS, bp)?
                }
                TokenKind::Multiply => {
                    self.parse_binary_expr(lhs, Expression::Multiplication, BP_MULT, bp)?
                }
                TokenKind::Divide => {
                    self.parse_binary_expr(lhs, Expression::Division, BP_DIV, bp)?
                }
                TokenKind::Modulo => self.parse_binary_expr(lhs, Expression::Modulo, BP_DIV, bp)?,
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
                TokenKind::Question => {
                    if bp >= BP_QUESTION {
                        lhs
                    } else {
                        let t = self.expect_token()?;

                        let loc = wrap_locations(lhs.loc, t.loc);
                        let expr = Src::new(Expression::Try(Box::new(lhs)), loc);
                        // XXX Continuation

                        expr
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
                                        arguments: CallExpression::DestructureImplicit(a.loc)
                                            .into(),
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
                                        arguments: CallExpression::Destructure(expr).into(),
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
                                    arguments: CallExpression::Inline(arguments).into(),
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

    fn parse_binary_expr<F>(
        &mut self,
        lhs: Src<Expression>,
        factory: F,
        bp_op: u32,
        bp: u32,
    ) -> Result<Src<Expression>>
    where
        F: FnOnce(Box<Src<Expression>>, Box<Src<Expression>>) -> Expression,
    {
        Ok(if bp >= bp_op {
            lhs
        } else {
            self.iter.next();
            let rhs = self.parse_expression(bp_op)?;
            let loc = wrap_locations(lhs.loc, rhs.loc);
            let expr = Src::new(factory(lhs.into(), rhs.into()), loc);
            self.parse_continuation(expr, bp)?
        })
    }

    fn parse_params(&mut self, until: TokenKind) -> Result<Vec<ParamExpression>> {
        let mut params = Vec::new();

        loop {
            self.consume_whitespace();
            let Some(next) = self.iter.peek() else {
                return Err(self.fail_at_end("Unexpected end of input"));
            };
            if **next == until {
                break;
            }

            if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
                let name = name.clone();
                let t = self.expect_token()?;

                if self.next_if_kind(&TokenKind::Colon).is_some() {
                    let type_expr = self.parse_type_expr()?;
                    params.push(self.complete_param_expr(Some(name), type_expr)?);
                } else {
                    let type_expr = Src::new(TypeExpression::Scalar(name), t.loc);
                    let type_expr = self.parse_type_suffix(type_expr)?;

                    params.push(self.complete_param_expr(None, type_expr)?);
                }
            } else {
                let type_expr = self.parse_type_expr()?;
                params.push(self.complete_param_expr(None, type_expr)?);
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

    fn complete_param_expr(
        &mut self,
        name: Option<Ident>,
        type_expr: Src<TypeExpression>,
    ) -> Result<ParamExpression> {
        let mut attrs = Vec::new();
        while let Some(attr) = self.try_parse_type_attr()? {
            attrs.push(attr);
        }

        Ok(ParamExpression {
            name,
            type_expr,
            attrs,
        })
    }

    fn try_parse_type_attr(&mut self) -> Result<Option<Src<AttributeExpression>>> {
        if let Some(t) = self.next_if_kind(&TokenKind::Alpha) {
            let (name, _) = self.expect_ident()?;
            if let Some(l) = self.next_if_kind(&TokenKind::LeftParen) {
                let args = self.parse_args(l.loc)?;

                let loc = wrap_locations(t.loc, args.loc);
                return Ok(Some(Src::new(
                    AttributeExpression {
                        name,
                        args: Some(args),
                    },
                    loc,
                )));
            }
        }

        Ok(None)
    }

    fn parse_expressions(&mut self, until: TokenKind) -> Result<Vec<Src<Expression>>> {
        self.parse_list(until, |p| p.parse_expression(0))
    }

    fn parse_assignee(&mut self) -> Result<Src<Assignee>> {
        let token = self.expect_token()?;
        match token.as_ref() {
            TokenKind::Identifier(ident) => {
                let assignee = Assignee::scalar(ident.clone());
                Ok(Src::new(assignee, token.loc))
            }
            TokenKind::LeftParen => self.parse_destructuring_pattern(None, &token),
            _ => todo!("Expected assignment"),
        }
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
        self.parse_list(TokenKind::RightBrace, |p| {
            let (name, _) = p.expect_ident()?;

            if p.next_if_kind(&TokenKind::LeftParen).is_some() {
                let params = p.parse_params(TokenKind::RightParen)?;
                let r = p.expect_kind(TokenKind::RightParen)?;

                Ok(Variant {
                    name,
                    params: Some(params),
                })
            } else {
                Ok(Variant { name, params: None })
            }
        })
    }

    fn parse_args(&mut self, start_loc: Loc) -> Result<Src<Vec<ArgumentExpression>>> {
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
                    args.push(ArgumentExpression::named(name.clone(), value));
                } else {
                    let expr = self.handle_identifier_expr(name.clone(), t.loc, 0)?;

                    args.push(ArgumentExpression::unnamed(expr));
                }
            } else {
                let expr = self.parse_expression(0)?;
                args.push(ArgumentExpression::unnamed(expr));
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
        Ok(Src::new(args, loc))
    }

    fn parse_type_expr(&mut self) -> Result<Src<TypeExpression>> {
        if let Some(l) = self.next_if_kind(&TokenKind::LeftSquare) {
            let inner = self.parse_type_expr()?;
            let r = self.expect_kind(TokenKind::RightSquare)?;

            let kind = TypeExpression::List(inner.into());
            let expr = Src::new(kind, wrap_locations(l.loc, r.loc));
            self.parse_type_suffix(expr)
        } else if let Some(l) = self.next_if_kind(&TokenKind::LeftParen) {
            let params = self.parse_params(TokenKind::RightParen)?;
            let r = self.expect_kind(TokenKind::RightParen)?;

            let kind = TypeExpression::Tuple(params);
            let expr = Src::new(kind, wrap_locations(l.loc, r.loc));
            self.parse_type_suffix(expr)
        } else {
            let (raw, loc) = self.expect_ident()?;
            let kind = TypeExpression::Scalar(raw);
            let expr = Src::new(kind, loc);
            self.parse_type_suffix(expr)
        }
    }

    fn parse_type_suffix(&mut self, expr: Src<TypeExpression>) -> Result<Src<TypeExpression>> {
        if let Some(t) = self.next_if_kind(&TokenKind::Question) {
            let loc = wrap_locations(expr.loc, t.loc);
            let expr = Src::new(TypeExpression::Opt(expr.into()), loc);
            Ok(expr)
        } else {
            Ok(expr)
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
            if **next == until {
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
        if *token == kind {
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

    // TODO Reuse this pattern with other assignment cases
    fn peek_is_assignee_followed_by_in(&mut self) -> bool {
        if let Some(TokenKind::Identifier(_)) = self.peek_kind()
            && let Some(TokenKind::In) = self.peek_kind_nth(1)
        {
            return true;
        }
        if let Some(TokenKind::LeftParen) = self.peek_kind()
            && let Some(TokenKind::In) = self.peek_after_paren_nth(1)
        {
            return true;
        }

        false
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.iter.peek().map(|t| t.as_ref())
    }

    fn peek_kind_nth(&mut self, n: usize) -> Option<&TokenKind> {
        self.iter.peek_nth(n).map(|t| t.as_ref())
    }

    fn peek_after_paren(&mut self) -> Option<&TokenKind> {
        self.peek_after_paren_nth(0)
    }

    fn peek_after_paren_nth(&mut self, nth: usize) -> Option<&TokenKind> {
        let mut d = 1;
        for n in nth.. {
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
        // use std::backtrace::Backtrace;
        // println!("{}", Backtrace::force_capture());
        Error::new(msg.into(), self.src, loc)
    }

    fn wrap_err(&self, err: Error, offset: usize) -> Error {
        Error::new(err.message, self.src, err.loc.shift_right(offset))
    }

    fn parse_string(&self, src: &str, loc: Loc) -> Result<Src<Expression>> {
        let parts = interpopation::tokenise_string(src);

        if parts.is_empty() {
            return Ok(Src::new(Expression::Str("".into()), loc));
        }

        let mut res = Vec::new();

        let start_offset = loc.start + 1;
        for part in parts {
            let expr = match &part.kind {
                StringTokenKind::Str => Src::new(
                    Expression::Str(part.src.into()),
                    Loc::new(0, part.src.len()),
                ),
                StringTokenKind::Expr => {
                    let tokens = lexer::tokenize(part.src)
                        .map_err(|err| self.wrap_err(err, start_offset + part.offset))?;
                    Parser::new(part.src, tokens)
                        .parse_single_expression()
                        .map_err(|err| self.wrap_err(err, start_offset + part.offset))?
                }
            };

            res.push((expr, start_offset + part.offset));
        }

        Ok(Src::new(Expression::String(res), loc))
    }
}

fn wrap_locations(start: Loc, end: Loc) -> Loc {
    Loc::new(
        cmp::min(start.start, end.start),
        cmp::max(start.end, end.end),
    )
}
