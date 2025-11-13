use std::{collections::HashMap, ops::Range, sync::Arc};

use crate::{
    error::{Error, Result},
    parser::{AstNode, Expression, ExpressionKind, Parameter},
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
    Record {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
    },
    RecordInstance {
        name: Arc<str>,
        params: Vec<(Arc<str>, ScriptType)>,
    },
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
                AstNode::Function {
                    name,
                    fun,
                    type_expr,
                } => {
                    let params = self.eval_params(&fun.params, &scope)?;
                    let ret = match type_expr {
                        Some(expr) => self.eval_type_expr(expr.as_ref(), &scope)?,
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
                        ScriptType::Record {
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
                        _ => todo!("Unexpected list type"),
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
                            self.fail(format!("Expected boolean, found {typ:?}"), &cond.loc)
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
                                    self.fail(format!("Expected {r:?}, found {typ:?}"), &expr.loc)
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
                let typ = self.eval_type_expr(p.type_expr.as_ref(), scope)?;
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

                let inner_type = types.first().cloned().unwrap_or(ScriptType::Void);
                for (typ, expr) in types.iter().zip(expressions) {
                    if *typ != inner_type {
                        return Err(
                            self.fail(format!("Expected {inner_type:?}, found {typ:?}"), &expr.loc)
                        );
                    }
                }

                Ok(ScriptType::List(inner_type.into()))
            }
            ExpressionKind::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => Ok(value.clone()),
            },
            ExpressionKind::Access { subject, key } => {
                let subject_typ = self.validate_expr(subject, scope)?;
                match subject_typ {
                    ScriptType::RecordInstance { name, params } => {
                        match params.iter().find(|(k, _)| *k == *key) {
                            None => {
                                Err(self
                                    .fail(format!("No such property: {key} on {name}"), &expr.loc))
                            }
                            Some((_, typ)) => Ok(typ.clone()),
                        }
                    }
                    _ => panic!("Unexpected property access on {subject:?}"),
                }
            }
            ExpressionKind::Not(expr) => {
                let typ = self.validate_expr(expr, scope)?;
                if typ != ScriptType::Bool {
                    return Err(self.fail(format!("Expected boolean, found {typ:?}"), &expr.loc));
                }
                Ok(ScriptType::Bool)
            }
            ExpressionKind::Equal(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l:?}, found {r:?}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if r != l {
                    return Err(self.fail(format!("Expected {l:?}, found {r:?}"), &rhs.loc));
                }

                Ok(ScriptType::Bool)
            }
            ExpressionKind::Range(lhs, rhs) => {
                let l = self.validate_expr(lhs, scope)?;
                let r = self.validate_expr(rhs, scope)?;

                if l != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {l:?}"), &lhs.loc));
                }
                if r != ScriptType::Int {
                    return Err(self.fail(format!("Expected number, found {r:?}"), &rhs.loc));
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
                                    self.fail(format!("Expected string, found {typ:?}"), &arg.loc)
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

                        // XXX Should be a function returning this type
                        Ok(ScriptType::State(typ.into()))
                    }
                    _ => match scope.locals.get(name) {
                        None => {
                            Err(self.fail(format!("Undefined reference: {name}"), &subject.loc))
                        }
                        Some(ScriptType::Function { params, ret }) => {
                            self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                            Ok(*ret.clone())
                        }
                        Some(ScriptType::Record { params, name }) => {
                            self.validate_args(params, args, kwargs, scope, &expr.loc)?;

                            Ok(ScriptType::RecordInstance {
                                params: params.clone(),
                                name: Arc::clone(name),
                            })
                        }
                        _ => panic!("Function not found {name:?}"),
                    },
                },
                ExpressionKind::Access { subject, key } => {
                    let subject_typ = self.validate_expr(subject, scope)?;
                    match subject_typ {
                        ScriptType::State(typ) => match key.as_ref() {
                            "get" => Ok(*typ),
                            "set" => {
                                // XXX Simulate normal function call
                                let params = vec![("".into(), *typ)];
                                self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                                Ok(ScriptType::Void)
                            }
                            _ => panic!("Unknown method: {key}"),
                        },
                        ScriptType::List(typ) => match key.as_ref() {
                            "push" => {
                                let params = vec![("".into(), *typ.clone())];
                                self.validate_args(&params, args, kwargs, scope, &expr.loc)?;
                                Ok(ScriptType::List(typ))
                            }
                            _ => panic!("Unknown method: {key}"),
                        },
                        _ => todo!("User-defined methods"),
                    }
                }
                _ => panic!("Call on something that's not a ref"),
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
            return Err(self.fail(format!("Expected number, found {l:?}"), &lhs.loc));
        }
        if r != ScriptType::Int {
            return Err(self.fail(format!("Expected number, found {r:?}"), &rhs.loc));
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
        // FIXME This should already be checked during type checking phase
        if params.len() != args.len() + kwargs.len() {
            let msg = format!("Expected {} arguments, found {}", params.len(), args.len());
            return Err(self.fail(msg, loc));
        }

        for ((_, param_typ), expr) in params.iter().zip(args) {
            let typ = self.validate_expr(expr, scope)?;
            if typ != *param_typ {
                let msg = format!("Expected {param_typ:?}, found {typ:?}");
                return Err(self.fail(msg, &expr.loc));
            }
        }

        // Match kwargs against remaining arguments
        let rest = &params[args.len()..];
        for (name, param_typ) in rest {
            if let Some((_, expr)) = kwargs.iter().find(|(n, _)| *n == *name) {
                let typ = self.validate_expr(expr, scope)?;
                if typ != *param_typ {
                    return Err(
                        self.fail(format!("Expected {param_typ:?}, found {typ:?}"), &expr.loc)
                    );
                }
            } else {
                return Err(self.fail(format!("Missing argument: {}", name), loc));
            }
        }

        Ok(())
    }

    fn eval_type_expr(&self, type_expr: &str, scope: &Scope) -> Result<ScriptType> {
        let typ = match type_expr {
            "str" => ScriptType::Str,
            "int" => ScriptType::Int,
            "bool" => ScriptType::Bool,
            e => todo!("Unrecognized type expr: {e:?}"),
        };
        Ok(typ)
    }

    fn fail(&self, msg: String, loc: &Range<usize>) -> Error {
        Error::new(msg, self.src, loc)
    }
}
