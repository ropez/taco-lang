use std::{
    collections::HashMap,
    io::Write,
    sync::{Arc, Mutex},
};

use crate::parser::{AstNode, Expression, Function};

#[derive(Debug)]
pub enum ScriptValue {
    Void,
    Boolean(bool),
    String(Arc<str>),
    Number(i64),
    Range(i64, i64),
    List(Vec<Arc<ScriptValue>>),
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => todo!()
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
    functions: HashMap<Arc<str>, (Arc<Function>, Scope)>,
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
                AstNode::Function { name, fun } => {
                    scope
                        .functions
                        .insert(Arc::clone(name), (Arc::clone(fun), scope.clone()));
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
                                scope.locals.insert(Arc::clone(ident), Arc::new(ScriptValue::Number(v)));
                                self.eval_block(body, scope);
                            }
                        }
                        _ => panic!("Expected iterable, found: {iterable:?}"),
                    }
                }
                AstNode::Condition { cond, body, else_body } => {
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
        match expr {
            Expression::String(s) => Arc::new(eval_str(s.clone(), scope)),
            Expression::Number(n) => Arc::new(ScriptValue::Number(*n)),
            Expression::List(s) => Arc::new(ScriptValue::List(
                s.iter().map(|i| self.eval_expr(i, scope)).collect(),
            )),
            Expression::Ref(ident) => match scope.locals.get(ident) {
                None => panic!("Undefined reference: {ident}"),
                Some(value) => Arc::clone(value),
            },
            Expression::Not(expr) => {
                let val = self.eval_expr(expr, scope);
                if let ScriptValue::Boolean(b) = *val {
                    Arc::new(ScriptValue::Boolean(!b))
                } else {
                    panic!("Not a boolean")
                }
            }
            Expression::Equal(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(lhs.eq(&rhs)))
            }
            Expression::NotEqual(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                Arc::new(ScriptValue::Boolean(!lhs.eq(&rhs)))
            }
            Expression::Range(lhs, rhs) => {
                let lhs = self.eval_expr(lhs, scope);
                let rhs = self.eval_expr(rhs, scope);

                match (lhs.as_ref(), rhs.as_ref()) {
                    (ScriptValue::Number(lhs), ScriptValue::Number(rhs)) =>
                        Arc::new(ScriptValue::Range(*lhs, *rhs)),
                    _ => panic!("Expected numbers in range"),
                }
            }
            Expression::Call { subject, arguments } => match subject.as_ref() {
                "println" => {
                    for arg in arguments {
                        let val = self.eval_expr(arg, scope);
                        match *val {
                            ScriptValue::String(ref s) => {
                                let mut out = self.stdout.lock().unwrap();
                                writeln!(out, "{s}").unwrap()
                            }
                            _ => panic!("Unexpected argument: {val:?}"),
                        }
                    }
                    Arc::new(ScriptValue::Void)
                }
                // FIXME: Replace `push(l, i)` with `l.push(i)`
                "push" => {
                    let list = arguments.get(0).expect("push list");
                    let item = arguments.get(1).expect("push item");
                    let res = match *self.eval_expr(list, scope) {
                        ScriptValue::List(ref l) => {
                            let value = self.eval_expr(item, scope);
                            // This is the Copy on write
                            // Can we avoid copy, if we see that the original will not be used again?
                            let mut res = l.clone();
                            res.push(value);
                            ScriptValue::List(res)
                        }
                        ref e => panic!("Expected list, found: {e:?}"),
                    };
                    Arc::new(res)
                }
                _ => {
                    match scope.functions.get(subject) {
                        None => panic!("Undefined function: {subject:?}"),
                        Some((fun, captured_scope)) => {
                            // FIXME This should already be checked during type checking phase
                            if fun.params.len() != arguments.len() {
                                panic!(
                                    "Expected {} arguments, found {}",
                                    fun.params.len(),
                                    arguments.len()
                                );
                            }

                            let mut inner_scope = captured_scope.clone();

                            for (ident, arg) in fun.params.iter().zip(arguments) {
                                let val = self.eval_expr(arg, scope);
                                inner_scope.locals.insert(ident.clone(), val);
                            }

                            let c = self.eval_block(&fun.body, inner_scope);

                            match c {
                                Completion::EndOfBlock => Arc::new(ScriptValue::Void),
                                Completion::Return(v) => v,
                            }
                        }
                    }
                }
            },
            // _ => unimplemented!("Expression: {expr:?}"),
        }
    }
}

fn eval_str(src: Arc<str>, scope: &Scope) -> ScriptValue {
    // We probably need the parser to play a role in this.
    // This naive implementation silently ignores missing references.
    let mut res = src.to_string();
    for (ident, value) in &scope.locals {
        let fmt = format!("${ident}");
        while res.contains(&fmt) {
            let val = match value.as_ref() {
                ScriptValue::String(s) => s.clone(),
                ScriptValue::Number(n) => n.to_string().into(),
                _ => panic!("Unexpected token in string interpolation!"),
            };
            res = res.replace(&fmt, &val);
        }
    }
    ScriptValue::String(res.into())
}
