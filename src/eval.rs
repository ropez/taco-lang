use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as _},
    io::Write,
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::parser::{AstNode, Expression, ExpressionKind, Function, Parameter, Rec};

#[derive(Debug)]
pub enum ScriptValue {
    Void,
    Boolean(bool),
    String(Arc<str>),
    Number(i64),
    Range(i64, i64),
    List(Vec<Arc<ScriptValue>>),

    Rec {
        rec: Arc<Rec>,
        values: Vec<Arc<ScriptValue>>,
    },

    State(RwLock<Arc<ScriptValue>>),
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            _ => todo!(),
        }
    }
}

impl Display for ScriptValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::String(s) => write!(f, "{s}"),
            ScriptValue::Number(n) => write!(f, "{n}"),
            ScriptValue::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            _ => panic!("Unexpected token in string interpolation!"),
        }
    }
}

#[derive(Debug)]
pub enum Completion {
    EndOfBlock,
    Return(Arc<ScriptValue>),
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, Arc<ScriptValue>>,

    // XXX functions, records could all be script values in locals
    functions: HashMap<Arc<str>, (Arc<Function>, Scope)>,
    records: HashMap<Arc<str>, Arc<Rec>>,
}

pub struct Engine<O>
where
    O: Write,
{
    stdout: Mutex<O>,
}

impl<O> Engine<O>
where
    O: Write,
{
    pub fn new(stdout: O) -> Self {
        Self {
            stdout: Mutex::new(stdout),
        }
    }

    pub fn eval(&self, ast: &[AstNode]) {
        self.eval_block(ast, Scope::default());
    }

    fn eval_block(&self, ast: &[AstNode], mut scope: Scope) -> Completion {
        for node in ast {
            match node {
                AstNode::Assignment { name, value } => {
                    scope
                        .locals
                        .insert(Arc::clone(name), self.eval_expr(value, &scope));
                }
                AstNode::Function { name, fun, .. } => {
                    scope
                        .functions
                        .insert(Arc::clone(name), (Arc::clone(fun), scope.clone()));
                }
                AstNode::Rec(rec) => {
                    scope.records.insert(Arc::clone(&rec.name), Arc::clone(rec));
                }
                AstNode::Iteration {
                    ident,
                    iterable,
                    body,
                } => {
                    let iterable = self.eval_expr(iterable, &scope);
                    match *iterable {
                        ScriptValue::List(ref items) => {
                            for item in items {
                                let mut scope = scope.clone();
                                scope.locals.insert(Arc::clone(ident), Arc::clone(item));
                                self.eval_block(body, scope);
                            }
                        }
                        ScriptValue::Range(lhs, rhs) => {
                            for v in lhs..=rhs {
                                let mut scope = scope.clone();
                                scope
                                    .locals
                                    .insert(Arc::clone(ident), Arc::new(ScriptValue::Number(v)));
                                self.eval_block(body, scope);
                            }
                        }
                        _ => panic!("Expected iterable, found: {iterable:?}"),
                    }
                }
                AstNode::Condition {
                    cond,
                    body,
                    else_body,
                } => {
                    let val = self.eval_expr(cond, &scope);

                    if let ScriptValue::Boolean(b) = *val {
                        if b {
                            let scope = scope.clone();
                            self.eval_block(body, scope);
                        } else if let Some(else_body) = else_body {
                            let scope = scope.clone();
                            self.eval_block(else_body, scope);
                        }
                    } else {
                        panic!("Not a boolean");
                    }
                }
                AstNode::Expression(expr) => {
                    // HACK Evaluating expression for side-effects
                    // Should it be some restrictions on this?
                    // Only allowed for Void expressions?
                    self.eval_expr(expr, &scope);
                }
                AstNode::Return(expr) => {
                    let val = self.eval_expr(expr, &scope);
                    return Completion::Return(val);
                }
            }
        }

        Completion::EndOfBlock
    }

    fn eval_expr(&self, expr: &Expression, scope: &Scope) -> Arc<ScriptValue> {
        match &expr.kind {
            ExpressionKind::String(s) => Arc::new(ScriptValue::String(Arc::clone(s))),
            ExpressionKind::StringInterpolate(parts) => {
                // TODO Lazy evaluation (StringInterpolate ScriptValue variant with scope)
                let mut builder = String::new();
                for expr in parts {
                    let val = self.eval_expr(expr, scope);
                    write!(builder, "{val}").unwrap();
                }
                Arc::new(ScriptValue::String(builder.into()))
            }
            ExpressionKind::Number(n) => Arc::new(ScriptValue::Number(*n)),
            ExpressionKind::True => Arc::new(ScriptValue::Boolean(true)),
            ExpressionKind::False => Arc::new(ScriptValue::Boolean(false)),
            ExpressionKind::List(s) => Arc::new(ScriptValue::List(
                s.iter().map(|i| self.eval_expr(i, scope)).collect(),
            )),
            ExpressionKind::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => Arc::clone(value),
            },
            ExpressionKind::Access { subject, key } => {
                let subject = self.eval_expr(subject, scope);
                match subject.as_ref() {
                    ScriptValue::Rec { rec, values } => {
                        let index = rec.params.iter().position(|p| p.name == *key);
                        match index {
                            None => panic!("Property not found: {key:?}"),
                            Some(index) => Arc::clone(&values[index]),
                        }
                    }
                    _ => panic!("Unexpected property access on {subject:?}"),
                }
            }
            ExpressionKind::Not(expr) => {
                let val = self.eval_expr(expr, scope);
                if let ScriptValue::Boolean(b) = *val {
                    Arc::new(ScriptValue::Boolean(!b))
                } else {
                    panic!("Not a boolean")
                }
            }
            ExpressionKind::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(lhs.eq(&rhs)))
            }
            ExpressionKind::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(!lhs.eq(&rhs)))
            }
            ExpressionKind::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Range(*lhs, *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Addition(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs + *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Subtraction(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs - *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Multiplication(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs * *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Division(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) => {
                        Arc::new(ScriptValue::Number(*lhs / *rhs))
                    }
                    _ => panic!("Expected numbers in range"),
                }
            }
            ExpressionKind::Call {
                subject,
                args,
                kwargs,
            } => match &subject.kind {
                ExpressionKind::Ref(name) => match name.as_ref() {
                    "print" => {
                        for arg in args {
                            let val = self.eval_expr(arg, scope);
                            let mut out = self.stdout.lock().unwrap();
                            write!(out, "{val}").unwrap();
                        }
                        Arc::new(ScriptValue::Void)
                    }
                    "println" => {
                        for arg in args {
                            let val = self.eval_expr(arg, scope);
                            let mut out = self.stdout.lock().unwrap();
                            writeln!(out, "{val}").unwrap();
                        }
                        Arc::new(ScriptValue::Void)
                    }
                    "state" => {
                        let arg = args.get(0).expect("state arg");
                        let value = self.eval_expr(arg, scope);
                        Arc::new(ScriptValue::State(RwLock::new(value)))
                    }
                    _ => {
                        if let Some((fun, captured_scope)) = scope.functions.get(name) {
                            let values = self.eval_args(&fun.params, args, kwargs, scope);

                            let mut inner_scope = captured_scope.clone();

                            for (ident, val) in fun.params.iter().zip(values) {
                                inner_scope.locals.insert(ident.name.clone(), val);
                            }

                            let c = self.eval_block(&fun.body, inner_scope);

                            match c {
                                Completion::EndOfBlock => Arc::new(ScriptValue::Void),
                                Completion::Return(v) => v,
                            }
                        } else if let Some(rec) = scope.records.get(name) {
                            let values = self.eval_args(&rec.params, args, kwargs, scope);

                            let instance = ScriptValue::Rec {
                                rec: Arc::clone(rec),
                                values,
                            };

                            Arc::new(instance)
                        } else {
                            panic!("Undefined function: {subject:?}")
                        }
                    }
                },
                ExpressionKind::Access { subject, key } => {
                    let subject = self.eval_expr(subject, scope);
                    match (subject.as_ref(), key.as_ref()) {
                        (ScriptValue::State(state), "get") => {
                            let v = state.read().unwrap();
                            Arc::clone(v.deref())
                        }
                        (ScriptValue::State(state), "set") => {
                            let val = args.first().expect("get arg");
                            let val = self.eval_expr(val, scope);
                            let mut v = state.write().unwrap();
                            *v = val;
                            Arc::new(ScriptValue::Void)
                        }
                        (ScriptValue::List(list), "push") => {
                            let item = args.first().expect("push item");
                            let value = self.eval_expr(item, scope);
                            // This is the Copy on Write feature of the language in play.
                            // Can we avoid copy, if we see that the original will not be used again?
                            let mut res = list.clone();
                            res.push(value);
                            Arc::new(ScriptValue::List(res))
                        }
                        (ScriptValue::Rec { rec, values }, "with") => {
                            let mut new_values = values.clone();

                            for (param, v) in rec.params.iter().zip(new_values.iter_mut()) {
                                if let Some((_, expr)) =
                                    kwargs.iter().find(|(k, _)| *k == param.name)
                                {
                                    *v = self.eval_expr(expr, scope);
                                }
                            }

                            let instance = ScriptValue::Rec {
                                rec: Arc::clone(rec),
                                values: new_values,
                            };
                            Arc::new(instance)
                        }
                        _ => panic!("Unknown method: {key} on {subject:?}"),
                    }
                }
                _ => panic!("Call on something that's not a ref"),
            },
            // _ => unimplemented!("Expression: {expr:?}"),
        }
    }

    fn eval_args(
        &self,
        params: &Vec<Parameter>,
        arguments: &Vec<Expression>,
        kwargs: &Vec<(Arc<str>, Expression)>,
        scope: &Scope,
    ) -> Vec<Arc<ScriptValue>> {
        // FIXME This should already be checked during type checking phase
        if params.len() != arguments.len() + kwargs.len() {
            panic!(
                "Expected {} arguments, found {}",
                params.len(),
                arguments.len()
            );
        }

        let mut values: Vec<_> = arguments.iter().map(|a| self.eval_expr(a, scope)).collect();

        for param in &params[arguments.len()..] {
            if let Some((_, value)) = kwargs.iter().find(|(name, _)| *name == param.name) {
                values.push(self.eval_expr(value, scope));
            } else {
                panic!("Missing argument: {}", param.name);
            }
        }

        values
    }
}
