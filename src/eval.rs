use std::{collections::HashMap, sync::Arc};

use crate::parser::{AstNode, Expression, Function};

#[derive(Debug)]
pub enum ScriptValue {
    String(Arc<str>),
    List(Vec<Arc<ScriptValue>>),
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, Arc<ScriptValue>>,
    functions: HashMap<Arc<str>, Arc<Function>>,
}

pub fn eval(ast: &[AstNode]) {
    eval_block(ast, Scope::default());
}

fn eval_block(ast: &[AstNode], mut scope: Scope) {
    for node in ast {
        match node {
            AstNode::Assignment { name, value } => {
                scope.locals.insert(Arc::clone(name), eval_expr(value, &scope));
            }
            AstNode::Function { name, fun } => {
                scope.functions.insert(Arc::clone(name), Arc::clone(fun));
            }
            AstNode::Call { subject, arguments } => match subject.as_ref() {
                "println" => {
                    for arg in arguments {
                        let val = eval_expr(arg, &scope);
                        match *val {
                            ScriptValue::String(ref s) => println!("{s}"),
                            _ => panic!("Unexpected argument: {arg:?}"),
                        }
                    }
                }
                _ => {
                    match scope.functions.get(subject) {
                        None => panic!("Undefined function: {subject:?}"),
                        Some(f) => {
                            // FIXME: Capture scope
                            let scope = Scope::default();
                            eval_block(&f.body, scope)
                        }
                    }
                }
            },
            AstNode::Iteration { ident, iterable, body } => {
                let iterable = eval_expr(iterable, &scope);
                match *iterable {
                    ScriptValue::List(ref items) => {
                        for item in items {
                            let mut scope = scope.clone();
                            scope.locals.insert(Arc::clone(ident), Arc::clone(item));
                            eval_block(body, scope);
                        }
                    }
                    _ => panic!("Expected list, found: {iterable:?}"),
                }
            },
        }
    }
}

fn eval_expr(expr: &Expression, scope: &Scope) -> Arc<ScriptValue> {
    match expr {
        Expression::String(s) => Arc::new(ScriptValue::String(Arc::clone(s))),
        Expression::List(s) => Arc::new(ScriptValue::List(s.iter().map(|i| eval_expr(i, scope)).collect())),
        Expression::Ref(ident) => match scope.locals.get(ident) {
            None => panic!("Undefined reference: {ident}"),
            Some(value) => Arc::clone(value),
        },
        // _ => unimplemented!("Expression: {expr:?}"),
    }
}
