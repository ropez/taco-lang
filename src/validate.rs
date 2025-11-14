use std::{collections::HashMap, fmt::Display, ops::Range, sync::Arc};

use crate::{
    error::{Error, Result},
    parser::{AstNode, Expression, ExpressionKind, Parameter, TypeExpression, TypeExpressionKind},
};

#[derive(Debug, PartialEq, Clone)]
enum ScriptType {
    Void,
    Bool,
    Int,
    Range,
    Str,
    State(Box<ScriptType>),
    List(Box<ScriptType>),
    Function {
        params: Vec<(Arc<str>, ScriptType)>,
        ret: Box<ScriptType>,
    },
    // Maybe this shouldn't be a 'ScriptType'
    RecordDef {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
    },
    Record {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
    },
}

// TODO Validate that all branches in non-void functions return something
// TODO Validate unreachable code?

impl Display for ScriptType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str => {
                write!(f, "str")
            }
            Self::List(inner) if *inner.as_ref() == Self::Void => {
                write!(f, "[]")
            }
            Self::List(inner) => {
                write!(f, "[{inner}]")
            }
            _ => write!(f, "{:?}", self)
        }
    }
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, ScriptType>,
    ret: Option<ScriptType>,
}

struct Validator<'a> {
    src: &'a str,
}

pub fn validate(src: &str, ast: &[AstNode]) -> Result<()> {
    let validator = Validator { src };
    validator.validate_block(ast, Scope::default())
}

impl<'a> Validator<'a> {
    fn validate_block(&self, ast: &[AstNode], mut scope: Scope) -> Result<()> {
        for node in ast {
            match node {
                AstNode::Assignment { name, value } => {
                    let typ = self.validate_expr(value, &scope)?;
                    if typ == ScriptType::Void {
                        return Err(
                            self.fail("Expected a value, found Void expression".into(), &value.loc)
                        );
                    }
                    scope.locals.insert(Arc::clone(name), typ);
                }
                AstNode::Function { name, fun } => {
                    let params = self.eval_params(&fun.params, &scope)?;
                    let ret = match &fun.type_expr {
                        Some(expr) => self.eval_type_expr(expr, &scope)?,
                        None => ScriptType::Void,
                    };

                    let mut inner = scope.clone();
                    for (name, typ) in &params {
                        inner.locals.insert(Arc::clone(name), typ.clone());
                    }
                    inner.ret = Some(ret.clone());
                    self.validate_block(&fun.body, inner)?;

                    scope.locals.insert(
                        Arc::clone(name),
                        ScriptType::Function {
                            params,
                            ret: ret.into(),
                        },
                    );
                }
                AstNode::Record(rec) => {
                    let params = self.eval_params(&rec.params, &scope)?;

                    scope.locals.insert(
                        Arc::clone(&rec.name),
                        ScriptType::RecordDef {
                            params,
                            name: Arc::clone(&rec.name),
                        },
                    );
                }
                AstNode::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable_typ = self.validate_expr(iterable, &scope)?;

                    match iterable_typ {
                        ScriptType::List(inner) => {
                            let mut inner_scope = scope.clone();
                            inner_scope.locals.insert(Arc::clone(ident), *inner);
                            self.validate_block(body, inner_scope)?;
                        }
                        ScriptType::Range => {
                            let mut inner_scope = scope.clone();
                            inner_scope
                                .locals
                                .insert(Arc::clone(ident), ScriptType::Int);
                            self.validate_block(body, inner_scope)?;
                        }
                        _ => {
                            let msg = format!("Expected iterable, found {iterable_typ}");
                            return Err(self.fail(msg, &iterable.loc));
                        }
                    }
                }
                AstNode::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let typ = self.validate_expr(cond, &scope)?;
                    if typ != ScriptType::Bool {
                        return Err(
                            self.fail(format!("Expected boolean, found {typ}"), &cond.loc)
                        );
                    }

                    self.validate_block(body, scope.clone())?;

                    if let Some(else_body) = else_body.as_ref() {
                        self.validate_block(else_body, scope.clone())?;
                    }
                }
                AstNode::Expression(expr) => {
                    self.validate_expr(expr, &scope)?;
                }
                AstNode::Return(expr) => {
                    let typ = self.validate_expr(expr, &scope)?;
                    match &scope.ret {
                        None => return Err(self.fail("Unexpected return value".into(), &expr.loc)),
                        Some(r) => {
                            if typ != *r {
                                return Err(
                                    self.fail(format!("Expected {r}, found {typ}"), &expr.loc)
                                );
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn eval_params(
        &self,
        params: &[Parameter],
        scope: &Scope,
    ) -> Result<Vec<(Arc<str>, ScriptType)>> {
        params
            .iter()
            .map(|p| {
                let typ = self.eval_type_expr(&p.type_expr, scope)?;
                Ok((Arc::clone(&p.name), typ))
            })
            .collect()
    }

    fn validate_expr(&self, expr: &Expression, scope: &Scope) -> Result<ScriptType> {
        match &expr.kind {
            ExpressionKind::Number(_) => Ok(ScriptType::Int),
            ExpressionKind::String(_) => Ok(ScriptType::Str),
            ExpressionKind::True => Ok(ScriptType::Bool),
            ExpressionKind::False => Ok(ScriptType::Bool),
            ExpressionKind::StringInterpolate(parts) => {
                // TODO Validate internal expressions
                Ok(ScriptType::Str)
                // let mut builder = String::new();
                // for expr in parts {
                //     let val = self.eval_expr(expr, scope);
                //     write!(builder, "{val}").unwrap();
                // }
                // Arc::new(ScriptValue::String(builder.into()))
            }
            ExpressionKind::List(expressions) => {
                let types = expressions
                    .iter()
                    .map(|i| self.validate_expr(i, scope))
                    .collect::<Result<Vec<ScriptType>>>()?;

                // XXX How to define type for empty list?

                let inner_type = types.first().cloned().unwrap_or(ScriptType::Void);
                for (typ, expr) in types.iter().zip(expressions) {
                    if *typ != inner_type {
                        return Err(
                            self.fail(format!("Expected {inner_type}, found {typ}"), &expr.loc)
                        );
                    }
                }

                Ok(ScriptType::List(inner_type.into()))
            }
            ExpressionKind::Ref(ident) => match scope.locals.get(ident) {
                None => Err(self.fail(format!("Undefined reference: {ident}"), &expr.loc)),
                Some(value) => Ok(value.clone()),
            },
            ExpressionKind::Access { subject, key } => {
                let subject_typ = self.validate_expr(subject, scope)?;
                match &subject_typ {
                    ScriptType::Record { params, .. } => {
                        match params.iter().find(|(k, _)| *k == *key) {
                            Some((_, typ)) => Ok(typ.clone()),
                            None => Err(self.fail(
                                format!("Unknown attribute: {key} on {subject_typ}"),
                                &expr.loc,
                            )),
                        }
                    }
                    _ => Err(self.fail(
                        format!("Unknown attribute: {key} on {subject_typ}"),
                        &expr.loc,
                    )),
                }
            }
            ExpressionKind::Not(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if typ != ScriptType::Bool {
                    return Err(self.fail(format!("Expected boolean, found {typ}"), &expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            ExpressionKind::Equal(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l}, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l}, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if l != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {l}"), &lhs.loc));
                }
                if r != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {r}"), &rhs.loc));
                }

                Ok(ScriptType::Range)
            }
            ExpressionKind::Addition(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Subtraction(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Multiplication(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Division(lhs, rhs) => self.validate_arithmetic(scope, lhs, rhs),
            ExpressionKind::Call {
                subject,
                args,
                kwargs,
            } => match &subject.kind {
                ExpressionKind::Ref(name) => match name.as_ref() {
                    "print" | "println" => {
                        for arg in args {
                            let typ = self.validate_expr(arg, scope)?;
                            if typ != ScriptType::Str {
                                return Err(
                                    self.fail(format!("Expected string, found {typ}"), &arg.loc)
                                );
                            }
                        }
                        Ok(ScriptType::Void)
                    }
                    "state" => {
                        if args.len() != 1 {
                            return Err(self.fail(
                                format!("Expected 1 argument, got {}", args.len()),
                                &expr.loc,
                            ));
                        }
                        let arg = args.first().expect("state arg");
                        let typ = self.validate_expr(arg, scope)?;

                        Ok(ScriptType::State(typ.into()))
                    }
                    _ => {
                        match scope.locals.get(name) {
                            None => {
                                Err(self.fail(format!("Undefined reference: {name}"), &subject.loc))
                            }
                            Some(ScriptType::Function { params, ret }) => {
                                self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                                Ok(*ret.clone())
                            }
                            Some(ScriptType::RecordDef { params, name }) => {
                                self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                                Ok(ScriptType::Record {
                                    params: params.clone(),
                                    name: Arc::clone(name),
                                })
                            }
                            Some(t) => Err(self
                                .fail(format!("Expected a callable, found {t}"), &subject.loc)),
                        }
                    }
                },
                ExpressionKind::Access { subject, key } => {
                    let subject_typ = self.validate_expr(subject, scope)?;
                    match (&subject_typ, key.as_ref()) {
                        (ScriptType::State(typ), "get") => Ok(*typ.clone()),
                        (ScriptType::State(typ), "set") => {
                            // XXX Simulate normal function call
                            let params = vec![("".into(), *typ.clone())];
                            self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                            Ok(ScriptType::Void)
                        }
                        (ScriptType::List(typ), "push") => {
                            // "Promote" list type, if empty list
                            let typ = match typ.as_ref() {
                                ScriptType::Void => {
                                    let t = args
                                        .first()
                                        .map(|i| self.validate_expr(i, scope))
                                        .transpose()?;

                                    t.unwrap()
                                }
                                _ => *typ.clone(),
                            };

                            let params = vec![("".into(), typ.clone())];
                            self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                            Ok(ScriptType::List(typ.into()))
                        }
                        (ScriptType::Record { params, .. }, "with") => {
                            // TODO: Check args
                            self.validate_kwargs(params, kwargs, scope)?;

                            Ok(subject_typ.clone())
                        }
                        _ => Err(self.fail(
                            format!("Unknown method: {key} on {subject_typ}"),
                            &expr.loc,
                        )),
                    }
                }
                _ => Err(self.fail("Call not allowed here".into(), &expr.loc)),
            },
        }
    }

    fn validate_arithmetic(
        &self,
        scope: &Scope,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<ScriptType> {
        let l = self.validate_expr(lhs, scope)?;
        let r = self.validate_expr(rhs, scope)?;

        if l != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {l}"), &lhs.loc));
        }
        if r != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {r}"), &rhs.loc));
        }

        Ok(ScriptType::Int)
    }

    fn validate_args(
        &self,
        params: &[(Arc<str>, ScriptType)],
        args: &[Expression],
        kwargs: &[(Arc<str>, Expression)],
        scope: &Scope,
        loc: &Range<usize>,
    ) -> Result<()> {
        // Check positional arguments
        for ((_, param_typ), expr) in params.iter().zip(args) {
            let typ = self.validate_expr(expr, scope)?;
            if typ != *param_typ {
                let msg = format!("Expected {param_typ}, found {typ}");
                return Err(self.fail(msg, &expr.loc));
            }
        }

        // Check number of positional args
        if args.len() > params.len() {
            return Err(self.fail(
                format!(
                    "Too many positional arguments: {} found, {} expected",
                    args.len(),
                    params.len()
                ),
                loc,
            ));
        }

        // Check kwargs against remaining params
        self.validate_kwargs(&params[args.len()..], kwargs, scope)?;

        // Check that all params are given
        for (name, _) in &params[args.len()..] {
            if !kwargs.iter().any(|(n, _)| *n == *name) {
                return Err(self.fail(format!("Missing argument: {}", name), loc));
            }
        }

        Ok(())
    }

    fn validate_kwargs(
        &self,
        params: &[(Arc<str>, ScriptType)],
        kwargs: &[(Arc<str>, Expression)],
        scope: &Scope,
    ) -> Result<()> {
        let mut found = Vec::new();

        for (name, expr) in kwargs {
            if found.contains(name) {
                return Err(self.fail(format!("Duplicate keyword argument: {name}"), &expr.loc));
            }
            found.push(Arc::clone(name));

            if let Some((_, param_typ)) = params.iter().find(|(n, _)| *n == *name) {
                let typ = self.validate_expr(expr, scope)?;
                if typ != *param_typ {
                    return Err(
                        self.fail(format!("Expected {param_typ}, found {typ}"), &expr.loc)
                    );
                }
            } else {
                return Err(self.fail(format!("Argument not found, {name}"), &expr.loc));
            }
        }

        Ok(())
    }

    fn eval_type_expr(&self, type_expr: &TypeExpression, scope: &Scope) -> Result<ScriptType> {
        match &type_expr.kind {
            TypeExpressionKind::Scalar(ident) => match ident.as_ref() {
                "str" => Ok(ScriptType::Str),
                "int" => Ok(ScriptType::Int),
                "bool" => Ok(ScriptType::Bool),
                e => match scope.locals.get(e) {
                    Some(ScriptType::RecordDef { name, params }) => Ok(ScriptType::Record {
                        params: params.clone(),
                        name: Arc::clone(name),
                    }),
                    _ => Err(self.fail(format!("Unknown type: {e}"), &type_expr.loc)),
                },
            },
            TypeExpressionKind::List(inner) => {
                let inner = self.eval_type_expr(inner.as_ref(), scope)?;
                Ok(ScriptType::List(inner.into()))
            }
        }
    }

    fn fail(&self, msg: String, loc: &Range<usize>) -> Error {
        Error::new(msg, self.src, loc)
    }
}
