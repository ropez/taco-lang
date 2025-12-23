use std::{
    cmp::{self},
    result,
    sync::Arc,
    vec::IntoIter,
};

use multipeek::{IteratorExt, MultiPeek};

use crate::{
    error::{ParseError, ParseErrorKind},
    ident::Ident,
    interpolation::{self, StringTokenKind},
    lexer::{self, Loc, Src, Token, TokenKind},
};

type Result<T> = result::Result<T, ParseError>;

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

    While {
        cond: Src<Expression>,
        body: Vec<Statement>,
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

    // Everything else is an expression
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
    Pipe(Box<Src<Expression>>, Box<Src<Expression>>),
    LogicNot(Box<Src<Expression>>),
    LogicAnd(Box<Src<Expression>>, Box<Src<Expression>>),
    LogicOr(Box<Src<Expression>>, Box<Src<Expression>>),
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
    // An expression followed by the '!' operator
    AssertSome(Box<Src<Expression>>),
    // An opt expression with a fallback (e.g. find() ?? 0)
    Coalesce(Box<Src<Expression>>, Box<Src<Expression>>),

    Call {
        subject: Box<Src<Expression>>,
        arguments: Box<CallExpression>,
    },
    Access {
        subject: Box<Src<Expression>>,
        key: Ident,
    },

    Function(Arc<Function>),

    Match {
        expr: Box<Src<Expression>>,
        arms: Vec<MatchArm>,
        is_opt: bool,
    },
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
    Int,
    Str,
    Bool,
    Range,
    List(Box<Src<TypeExpression>>),
    Tuple(Vec<ParamExpression>),
    Opt(Box<Src<TypeExpression>>),
    TypeName(Ident),
    Infer,
}

impl TypeExpression {
    pub(crate) fn from_ident(ident: Ident) -> Self {
        match ident.as_str() {
            "int" => Self::Int,
            "str" => Self::Str,
            "bool" => Self::Bool,
            "range" => Self::Range,
            _ => Self::TypeName(ident),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub(crate) params: Src<Vec<ParamExpression>>,
    pub(crate) type_expr: Option<Src<TypeExpression>>,
    pub(crate) body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Record {
    pub(crate) name: Ident,
    pub(crate) params: Src<Vec<ParamExpression>>,
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

impl Enumeration {
    pub(crate) fn find_variant(&self, name: &Ident) -> Option<(usize, &Variant)> {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, v)| v.name == *name)
    }
}

#[derive(Debug)]
pub struct Variant {
    pub(crate) name: Ident,
    pub(crate) params: Option<Src<Vec<ParamExpression>>>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub struct MatchArm {
    pub(crate) pattern: Box<Src<MatchPattern>>,
    pub(crate) expr: Box<Src<Expression>>,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    // XXX DRY this into some "LiteralExpression"?
    Discard,
    Assignee(Ident),
    True,
    False,
    Int(i64),
    Str(Arc<str>),
    EnumVariant(Option<Ident>, Ident, Option<Src<Assignee>>),
}

impl MatchPattern {
    pub(crate) fn matches(&self, other: &MatchPattern) -> bool {
        match self {
            Self::Discard => true,
            Self::Assignee(_) => !matches!(other, Self::Discard),
            Self::True => matches!(other, Self::True),
            Self::False => matches!(other, Self::False),
            _ => unreachable!("comparing patterns, {self:?}"),
        }
    }
}

pub struct Parser<'a> {
    src: &'a str,
    iter: MultiPeek<IntoIter<Token>>,
}

mod constants {
    pub(crate) const BP_PIPE: u32 = 1;
    pub(crate) const BP_LOGIC_OR: u32 = 7;
    pub(crate) const BP_LOGIC_AND: u32 = 8;
    pub(crate) const BP_EQUAL: u32 = 9;
    pub(crate) const BP_CMP: u32 = 10;
    pub(crate) const BP_SPREAD: u32 = 12;
    pub(crate) const BP_PLUS: u32 = 15;
    pub(crate) const BP_MINUS: u32 = 15;
    pub(crate) const BP_DIV: u32 = 20;
    pub(crate) const BP_MULT: u32 = 20;
    pub(crate) const BP_CALL: u32 = 90;
    pub(crate) const BP_NEGATE: u32 = 95;
    pub(crate) const BP_COALESCE: u32 = 99;
    pub(crate) const BP_ACCESS: u32 = 100;
    pub(crate) const BP_QUESTION: u32 = 110;
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
            return Err(ParseError::unexpected_token().at(token.loc));
        }

        Ok(expr)
    }

    pub fn parse_block(&mut self, root: bool) -> Result<Vec<Statement>> {
        let mut ast = Vec::new();

        if !root {
            self.expect_kind(TokenKind::LeftBrace)?;
        }

        while let Some(token) = self.discard_and_peek_next() {
            match token {
                TokenKind::RightBrace if !root => {
                    self.expect_token()?;
                    break;
                }
                TokenKind::Fun => {
                    self.expect_kind(TokenKind::Fun)?;
                    let (name, _) = self.expect_ident()?;
                    let params = self.parse_params(false)?;

                    let type_expr = if self.next_if_kind(&TokenKind::Colon).is_some() {
                        Some(self.parse_type_expr()?)
                    } else {
                        None
                    };

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
                    self.expect_kind(TokenKind::Return)?;
                    if let Some(TokenKind::NewLine | TokenKind::RightBrace) = self.peek_kind() {
                        ast.push(Statement::Return(None));
                    } else {
                        let expr = self.parse_expression(0)?;
                        self.expect_end_of_line()?;
                        ast.push(Statement::Return(Some(expr)));
                    }
                }
                TokenKind::Assert => {
                    self.expect_kind(TokenKind::Assert)?;
                    let expr = self.parse_expression(0)?;
                    self.expect_end_of_line()?;
                    ast.push(Statement::Assert(expr));
                }
                TokenKind::Identifier(name) => {
                    if let Some(TokenKind::Assign) = self.peek_kind_nth(1) {
                        let token = self.expect_token()?;
                        self.expect_kind(TokenKind::Assign)?;
                        let assignee = Assignee::scalar(name.clone());
                        let assignee = Src::new(assignee, token.loc);
                        let value = self.parse_expression(0)?;
                        ast.push(Statement::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let expr = self.parse_expression(0)?;
                        ast.push(Statement::Expression(expr));
                    }
                }
                TokenKind::LeftParen => {
                    if let Some(TokenKind::Assign) = self.peek_after_paren() {
                        let assignee = self.parse_destructuring_pattern(None)?;
                        self.expect_kind(TokenKind::Assign)?;
                        let value = self.parse_expression(0)?;
                        ast.push(Statement::Assignment { assignee, value });
                        self.expect_end_of_line()?;
                    } else {
                        let expr = self.parse_expression(0)?;
                        ast.push(Statement::Expression(expr));
                    }
                }
                TokenKind::If => {
                    self.expect_kind(TokenKind::If)?;
                    if self.peek_is_assignee_followed_by_in() {
                        let assignee = self.parse_assignee()?;
                        self.expect_kind(TokenKind::In)?;

                        let value = self.parse_expression(0)?;
                        let body = self.parse_block(false)?;

                        // XXX DRY
                        let else_body = self
                            .next_if_kind(&TokenKind::Else)
                            .map(|_| self.parse_block(false))
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
                        let body = self.parse_block(false)?;

                        let else_body = self
                            .next_if_kind(&TokenKind::Else)
                            .map(|_| self.parse_block(false))
                            .transpose()?;

                        ast.push(Statement::Condition {
                            cond,
                            body,
                            else_body,
                        });
                    }
                }
                TokenKind::While => {
                    self.expect_kind(TokenKind::While)?;
                    let cond = self.parse_expression(0)?;
                    let body = self.parse_block(false)?;

                    self.expect_end_of_line()?;

                    ast.push(Statement::While { cond, body });
                }
                TokenKind::For => {
                    self.expect_kind(TokenKind::For)?;
                    let (ident, _) = self.expect_ident()?;
                    self.expect_kind(TokenKind::In)?;
                    let iterable = self.parse_expression(0)?;
                    let body = self.parse_block(false)?;

                    self.expect_end_of_line()?;

                    ast.push(Statement::Iteration {
                        ident,
                        iterable,
                        body,
                    });
                }
                TokenKind::Rec => {
                    self.expect_kind(TokenKind::Rec)?;
                    let (name, _) = self.expect_ident()?;
                    let params = self.parse_params(false)?;
                    self.expect_end_of_line()?;

                    let rec = Arc::new(Record { name, params });
                    ast.push(Statement::Rec(rec));
                }
                TokenKind::Enum => {
                    self.expect_kind(TokenKind::Enum)?;
                    let (name, _) = self.expect_ident()?;
                    let variants = self.parse_variants()?;
                    self.expect_end_of_line()?;

                    let rec = Arc::new(Enumeration { name, variants });
                    ast.push(Statement::Enum(rec));
                }

                // Everything else is an expression
                _ => {
                    let expr = self.parse_expression(0)?;
                    ast.push(Statement::Expression(expr));
                }
            }
        }

        Ok(ast)
    }

    fn parse_expression(&mut self, bp: u32) -> Result<Src<Expression>> {
        let token = self.expect_token()?;

        let expr = match token.as_ref() {
            TokenKind::Identifier(s) => self.handle_identifier_expr(s.clone(), token.loc, bp)?,
            TokenKind::Exclamation => {
                // FIX continuation: !foo || bar
                let expr = self.parse_expression(bp)?;
                let loc = wrap_locations(token.loc, expr.loc);
                Src::new(Expression::LogicNot(expr.into()), loc)
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
                let params = self.parse_params(true)?;

                let type_expr = if self.next_if_kind(&TokenKind::Colon).is_some() {
                    Some(self.parse_type_expr()?)
                } else {
                    None
                };

                let body = self.parse_block(false)?;

                let loc = wrap_locations(token.loc, params.loc);
                let fun = Arc::new(Function {
                    body,
                    params,
                    type_expr,
                });

                let expr = Src::new(Expression::Function(fun), loc);
                self.parse_continuation(expr, bp)?
            }
            TokenKind::LeftBrace => {
                let expr = self.parse_expression(0)?;
                self.expect_kind(TokenKind::RightBrace)?;
                self.parse_continuation(expr, bp)?
            }
            TokenKind::LeftSquare => {
                let list =
                    self.parse_inner_list(TokenKind::RightSquare, |p| p.parse_expression(0))?;
                let e = self.expect_kind(TokenKind::RightSquare)?;
                let expr = Src::new(Expression::List(list), wrap_locations(token.loc, e.loc));
                self.parse_continuation(expr, 0)?
            }
            TokenKind::LeftParen => {
                let args = self.parse_inner_args()?;
                let e = self.expect_kind(TokenKind::RightParen)?;
                let args = Src::new(args, wrap_locations(token.loc, e.loc));
                let expr = Src::new(Expression::Tuple(args), wrap_locations(token.loc, e.loc));
                self.parse_continuation(expr, 0)?
            }
            TokenKind::Match => {
                let is_opt = self.next_if_kind(&TokenKind::Question).is_some();
                let expr = self.parse_expression(0)?;
                self.expect_kind(TokenKind::LeftBrace)?;
                let arms = self.parse_match_arms()?;
                let e = self.expect_kind(TokenKind::RightBrace)?;
                let expr = Src::new(
                    Expression::Match {
                        expr: expr.into(),
                        arms,
                        is_opt,
                    },
                    wrap_locations(token.loc, e.loc),
                );
                self.parse_continuation(expr, 0)?
            }
            _ => return Err(ParseError::unexpected_token().at(token.loc)),
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
            return Err(ParseError::expected("identifier").at(loc));
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

        let expr = match self.peek_continuation() {
            None => lhs,
            Some((new_line, kind)) => match (new_line, kind) {
                (_, TokenKind::Pipe) => {
                    if bp >= BP_PIPE {
                        lhs
                    } else {
                        self.expect_token()?;
                        let rhs = self.parse_expression(BP_PIPE)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Pipe(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                (_, TokenKind::LogicAnd) => {
                    self.parse_binary_expr(lhs, Expression::LogicAnd, BP_LOGIC_AND, bp)?
                }
                (_, TokenKind::LogicOr) => {
                    self.parse_binary_expr(lhs, Expression::LogicOr, BP_LOGIC_OR, bp)?
                }
                (_, TokenKind::Equal) => {
                    self.parse_binary_expr(lhs, Expression::Equal, BP_EQUAL, bp)?
                }
                (_, TokenKind::NotEqual) => {
                    self.parse_binary_expr(lhs, Expression::NotEqual, BP_EQUAL, bp)?
                }
                (_, TokenKind::LessThan) => {
                    self.parse_binary_expr(lhs, Expression::LessThan, BP_CMP, bp)?
                }
                (_, TokenKind::GreaterThan) => {
                    self.parse_binary_expr(lhs, Expression::GreaterThan, BP_CMP, bp)?
                }
                (_, TokenKind::LessOrEqual) => {
                    self.parse_binary_expr(lhs, Expression::LessOrEqual, BP_CMP, bp)?
                }
                (_, TokenKind::GreaterOrEqual) => {
                    self.parse_binary_expr(lhs, Expression::GreaterOrEqual, BP_CMP, bp)?
                }
                (_, TokenKind::Plus) => {
                    self.parse_binary_expr(lhs, Expression::Addition, BP_PLUS, bp)?
                }
                (_, TokenKind::Minus) => {
                    self.parse_binary_expr(lhs, Expression::Subtraction, BP_MINUS, bp)?
                }
                (_, TokenKind::Multiply) => {
                    self.parse_binary_expr(lhs, Expression::Multiplication, BP_MULT, bp)?
                }
                (_, TokenKind::Divide) => {
                    self.parse_binary_expr(lhs, Expression::Division, BP_DIV, bp)?
                }
                (_, TokenKind::Modulo) => {
                    self.parse_binary_expr(lhs, Expression::Modulo, BP_DIV, bp)?
                }
                (_, TokenKind::Dot) => {
                    if bp >= BP_ACCESS {
                        lhs
                    } else {
                        self.expect_token()?;
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
                (_, TokenKind::Question) => {
                    if bp >= BP_QUESTION {
                        lhs
                    } else {
                        let t = self.expect_token()?;

                        let loc = wrap_locations(lhs.loc, t.loc);
                        let expr = Src::new(Expression::Try(Box::new(lhs)), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                (_, TokenKind::Exclamation) => {
                    if bp >= BP_QUESTION {
                        lhs
                    } else {
                        let t = self.expect_token()?;

                        let loc = wrap_locations(lhs.loc, t.loc);
                        let expr = Src::new(Expression::AssertSome(Box::new(lhs)), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                (_, TokenKind::Coalesce) => {
                    if bp >= BP_COALESCE {
                        lhs
                    } else {
                        let _ = self.expect_token()?;
                        let rhs = self.parse_expression(BP_COALESCE)?;

                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr =
                            Src::new(Expression::Coalesce(Box::new(lhs), Box::new(rhs)), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                (_, TokenKind::Spread) => {
                    if bp >= BP_SPREAD {
                        lhs
                    } else {
                        self.expect_token()?;
                        let rhs = self.parse_expression(BP_SPREAD)?;
                        let loc = wrap_locations(lhs.loc, rhs.loc);
                        let expr = Src::new(Expression::Range(lhs.into(), rhs.into()), loc);
                        self.parse_continuation(expr, bp)?
                    }
                }
                (false, TokenKind::LeftParen) => {
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
                            let args = self.parse_inner_args()?;
                            let e = self.expect_kind(TokenKind::RightParen)?;
                            let arguments = Src::new(args, wrap_locations(t.loc, e.loc));

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
            self.expect_token()?;
            let rhs = self.parse_expression(bp_op)?;
            let loc = wrap_locations(lhs.loc, rhs.loc);
            let expr = Src::new(factory(lhs.into(), rhs.into()), loc);
            self.parse_continuation(expr, bp)?
        })
    }

    fn parse_params(&mut self, infer_types: bool) -> Result<Src<Vec<ParamExpression>>> {
        let l = self.expect_kind(TokenKind::LeftParen)?;

        let params = self.parse_inner_list(TokenKind::RightParen, |p| {
            if let Some(TokenKind::Identifier(name)) = p.peek_kind() {
                let name = name.clone();
                let t = p.expect_token()?;

                if p.next_if_kind(&TokenKind::Colon).is_some() {
                    let type_expr = p.parse_type_expr()?;
                    let param = p.complete_param_expr(Some(name), type_expr)?;
                    Ok(param)
                } else if infer_types {
                    let type_expr = Src::new(TypeExpression::Infer, t.loc);
                    let param = p.complete_param_expr(Some(name), type_expr)?;
                    Ok(param)
                } else {
                    let type_expr = Src::new(TypeExpression::from_ident(name), t.loc);
                    let type_expr = p.parse_type_suffix(type_expr)?;

                    let param = p.complete_param_expr(None, type_expr)?;
                    Ok(param)
                }
            } else {
                let type_expr = p.parse_type_expr()?;
                let param = p.complete_param_expr(None, type_expr)?;
                Ok(param)
            }
        })?;

        let r = self.expect_kind(TokenKind::RightParen)?;

        Ok(Src::new(params, wrap_locations(l.loc, r.loc)))
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
                let args = self.parse_inner_args()?;
                let e = self.expect_kind(TokenKind::RightParen)?;
                let args = Src::new(args, wrap_locations(l.loc, e.loc));

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

    fn parse_match_arms(&mut self) -> Result<Vec<MatchArm>> {
        let arms = self.parse_inner_list(TokenKind::RightBrace, |p| {
            let pattern = p.parse_match_pattern()?;
            p.expect_kind(TokenKind::LeftBrace)?;
            let expr = p.parse_expression(0)?;
            p.expect_kind(TokenKind::RightBrace)?;

            Ok(MatchArm {
                pattern: pattern.into(),
                expr: expr.into(),
            })
        })?;

        Ok(arms)
    }

    fn parse_match_pattern(&mut self) -> Result<Src<MatchPattern>> {
        let token = self.expect_token()?;
        let pattern = match token.as_ref() {
            TokenKind::True => Src::new(MatchPattern::True, token.loc),
            TokenKind::False => Src::new(MatchPattern::False, token.loc),
            TokenKind::Number(n) => Src::new(MatchPattern::Int(*n), token.loc),
            TokenKind::String(s) => Src::new(MatchPattern::Str(Arc::clone(s)), token.loc),
            TokenKind::Identifier(s) => {
                self.handle_identifier_match_pattern(s.clone(), token.loc)?
            }
            TokenKind::DoubleColon => {
                let (ident, e) = self.expect_ident()?;
                if let Some(TokenKind::LeftParen) = self.peek_kind() {
                    let pattern = self.parse_destructuring_pattern(None)?;
                    let loc = wrap_locations(token.loc, pattern.loc);
                    Src::new(MatchPattern::EnumVariant(None, ident, Some(pattern)), loc)
                } else {
                    let loc = wrap_locations(token.loc, e);
                    Src::new(MatchPattern::EnumVariant(None, ident, None), loc)
                }
            }
            _ => todo!("Invalid pattern"),
        };
        Ok(pattern)
    }

    fn handle_identifier_match_pattern(
        &mut self,
        ident: Ident,
        loc: Loc,
    ) -> Result<Src<MatchPattern>> {
        if ident.as_str() == "_" {
            return Ok(Src::new(MatchPattern::Discard, loc));
        }
        if self.next_if_kind(&TokenKind::DoubleColon).is_some() {
            let (name, l) = self.expect_ident()?;
            let expr = if let Some(TokenKind::LeftParen) = self.peek_kind() {
                let pattern = self.parse_destructuring_pattern(None)?;
                let loc = wrap_locations(loc, pattern.loc);
                Src::new(
                    MatchPattern::EnumVariant(Some(ident), name, Some(pattern)),
                    loc,
                )
            } else {
                let loc = wrap_locations(loc, l);
                Src::new(MatchPattern::EnumVariant(Some(ident), name, None), loc)
            };
            Ok(expr)
        } else {
            let expr = Src::new(MatchPattern::Assignee(ident), loc);
            Ok(expr)
        }
    }

    fn parse_assignee(&mut self) -> Result<Src<Assignee>> {
        match self.peek_kind_or_error()? {
            TokenKind::Identifier(ident) => {
                let token = self.expect_token()?;
                let assignee = Assignee::scalar(ident.clone());
                Ok(Src::new(assignee, token.loc))
            }
            TokenKind::LeftParen => self.parse_destructuring_pattern(None),
            _ => todo!("Expected assignment"),
        }
    }

    fn parse_destructuring_pattern(&mut self, name: Option<Ident>) -> Result<Src<Assignee>> {
        let t = self.expect_kind(TokenKind::LeftParen)?;
        let pattern = self.parse_inner_list(TokenKind::RightParen, |p| {
            if let Some(TokenKind::LeftParen) = p.peek_kind() {
                let assignee = p.parse_destructuring_pattern(None)?;
                Ok(assignee)
            } else {
                let (ident, loc) = p.expect_ident()?;
                if p.next_if_kind(&TokenKind::Colon).is_some() {
                    let assignee = p.parse_destructuring_pattern(Some(ident))?;
                    Ok(assignee)
                } else {
                    let assignee = Assignee::scalar(ident); // Discard if '_'
                    let assignee = Src::new(assignee, loc);
                    Ok(assignee)
                }
            }
        })?;

        let e = self.expect_kind(TokenKind::RightParen)?;
        let loc = wrap_locations(t.loc, e.loc);
        let assignee = Assignee::destructure(name, pattern);
        Ok(Src::new(assignee, loc))
    }

    fn parse_variants(&mut self) -> Result<Vec<Variant>> {
        self.expect_kind(TokenKind::LeftBrace)?;
        let variants = self.parse_inner_list(TokenKind::RightBrace, |p| {
            let (name, _) = p.expect_ident()?;

            if let Some(TokenKind::LeftParen) = p.peek_kind() {
                let params = p.parse_params(false)?;

                Ok(Variant {
                    name,
                    params: Some(params),
                })
            } else {
                Ok(Variant { name, params: None })
            }
        })?;
        self.expect_kind(TokenKind::RightBrace)?;

        Ok(variants)
    }

    fn parse_inner_args(&mut self) -> Result<Vec<ArgumentExpression>> {
        let args = self.parse_inner_list(TokenKind::RightParen, |p| {
            if let Some(TokenKind::Identifier(name)) = p.peek_kind() {
                let name = name.clone();
                let t = p.expect_token()?;

                if p.next_if_kind(&TokenKind::Colon).is_some() {
                    let value = p.parse_expression(0)?;
                    Ok(ArgumentExpression::named(name, value))
                } else {
                    let expr = p.handle_identifier_expr(name.clone(), t.loc, 0)?;

                    Ok(ArgumentExpression::unnamed(expr))
                }
            } else {
                let expr = p.parse_expression(0)?;
                Ok(ArgumentExpression::unnamed(expr))
            }
        })?;

        Ok(args)
    }

    fn parse_type_expr(&mut self) -> Result<Src<TypeExpression>> {
        if let Some(l) = self.next_if_kind(&TokenKind::LeftSquare) {
            let inner = self.parse_type_expr()?;
            let r = self.expect_kind(TokenKind::RightSquare)?;

            let kind = TypeExpression::List(inner.into());
            let expr = Src::new(kind, wrap_locations(l.loc, r.loc));
            self.parse_type_suffix(expr)
        } else if let Some(TokenKind::LeftParen) = self.peek_kind() {
            let params = self.parse_params(false)?;

            let loc = params.loc;
            let kind = TypeExpression::Tuple(params.into_inner());
            let expr = Src::new(kind, loc);
            self.parse_type_suffix(expr)
        } else {
            let (raw, loc) = self.expect_ident()?;
            let expr = Src::new(TypeExpression::from_ident(raw), loc);
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

    fn parse_inner_list<T, P>(&mut self, until: TokenKind, mut item_parser: P) -> Result<Vec<T>>
    where
        P: FnMut(&mut Self) -> Result<T>,
    {
        let mut items = Vec::new();

        loop {
            self.discard_whitespace();
            if self.peek_kind() == Some(&until) {
                break;
            }

            items.push(item_parser(self)?);

            let kind = self.peek_kind();
            match kind {
                None => break,
                Some(kind) if *kind == until => break,
                Some(TokenKind::Comma | TokenKind::NewLine) => {
                    self.iter.next();
                    continue;
                }
                _ => {
                    let token = self.expect_token()?;
                    return Err(ParseError::unexpected_token().at(token.loc));
                }
            }
        }

        Ok(items)
    }

    fn discard_whitespace(&mut self) {
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
                    _ => return Err(ParseError::unexpected_token().at(token.loc)),
                },
            }
        }

        Ok(())
    }

    fn expect_token(&mut self) -> Result<Token> {
        self.discard_whitespace();
        self.iter.next().ok_or_else(|| self.fail_at_end())
    }

    fn expect_kind(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.expect_token()?;
        if *token == kind {
            Ok(token)
        } else {
            Err(ParseError::expected_kind(kind).at(token.loc))
        }
    }

    fn expect_ident(&mut self) -> Result<(Ident, Loc)> {
        let token = self.expect_token()?;
        match token.as_ref() {
            TokenKind::Identifier(name) => Ok((name.clone(), token.loc)),
            _ => Err(ParseError::expected("identifier").at(token.loc)),
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
            && let Some(TokenKind::In) = self.peek_after_paren()
        {
            return true;
        }

        false
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.iter.peek().map(|t| t.as_ref())
    }

    fn peek_kind_or_error(&mut self) -> Result<TokenKind> {
        self.peek_kind().cloned().ok_or_else(|| self.fail_at_end())
    }

    // Skip newlines, and peek at the next "real" token
    fn discard_and_peek_next(&mut self) -> Option<TokenKind> {
        self.discard_whitespace();
        self.peek_kind().cloned()
    }

    fn peek_continuation(&mut self) -> Option<(bool, TokenKind)> {
        let mut new_line = false;
        for n in 0.. {
            match self.peek_kind_nth(n) {
                None => break,
                Some(TokenKind::NewLine) => {
                    new_line = true;
                }
                Some(TokenKind::Comment(_)) => {}
                Some(kind) => return Some((new_line, kind.clone())),
            }
        }

        None
    }

    fn peek_kind_nth(&mut self, n: usize) -> Option<&TokenKind> {
        self.iter.peek_nth(n).map(|t| t.as_ref())
    }

    fn peek_after_paren(&mut self) -> Option<&TokenKind> {
        let mut d = 0;
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

    fn fail_at_end(&self) -> ParseError {
        let len = self.src.len();
        ParseError::new(ParseErrorKind::UnexpectedEndOfInput).at(Loc::new(len - 1, len))
    }

    fn parse_string(&self, src: &str, loc: Loc) -> Result<Src<Expression>> {
        let parts = interpolation::tokenise_string(src);

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
                        .map_err(|err| err.shift_right(start_offset + part.offset))?;
                    Parser::new(part.src, tokens)
                        .parse_single_expression()
                        .map_err(|err| err.shift_right(start_offset + part.offset))?
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
