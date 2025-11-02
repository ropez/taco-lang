use std::{collections::HashMap, sync::Arc};

use crate::parser::{AstNode, Expression, Function};

#[derive(Debug)]
pub enum ScriptValue {
    Void,
    String(Arc<str>),
    List(Vec<Arc<ScriptValue>>),
}

#[derive(Default, Clone)]
struct Scope {
    locals: HashMap<Arc<str>, Arc<ScriptValue>>,
    functions: HashMap<Arc<str>, (Arc<Function>, Scope)>,
}

pub fn eval(ast: &[AstNode]) {
    eval_block(ast, Scope::default());
}

fn eval_block(ast: &[AstNode], mut scope: Scope) {
    for node in ast {
        match node {
            AstNode::Assignment { name, value } => {
                scope
                    .locals
                    .insert(Arc::clone(name), eval_expr(value, &scope));
            }
            AstNode::Function { name, fun } => {
                scope
                    .functions
                    .insert(Arc::clone(name), (Arc::clone(fun), scope.clone()));
            }
            AstNode::Expression(expr) => {
                // HACK Evaluating expression for side-effects
                // Should it be some restrictions on this?
                // Only allowed for Void expressions?
                eval_expr(expr, &scope);
            }
            AstNode::Iteration {
                ident,
                iterable,
                body,
            } => {
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
            }
        }
    }
}

fn eval_expr(expr: &Expression, scope: &Scope) -> Arc<ScriptValue> {
    match expr {
        Expression::String(s) => Arc::new(eval_str(s.clone(), scope)),
        Expression::List(s) => Arc::new(ScriptValue::List(
            s.iter().map(|i| eval_expr(i, scope)).collect(),
        )),
        Expression::Ref(ident) => match scope.locals.get(ident) {
            None => panic!("Undefined reference: {ident}"),
            Some(value) => Arc::clone(value),
        },
        Expression::Call { subject, arguments } => match subject.as_ref() {
            "println" => {
                for arg in arguments {
                    let val = eval_expr(arg, scope);
                    match *val {
                        ScriptValue::String(ref s) => println!("{s}"),
                        _ => panic!("Unexpected argument: {arg:?}"),
                    }
                }
                Arc::new(ScriptValue::Void)
            }
            // FIXME: Replace `push(l, i)` with `l.push(i)`
            "push" => {
                let list = arguments.get(0).expect("push list");
                let item = arguments.get(1).expect("push item");
                let res = match *eval_expr(list, scope) {
                    ScriptValue::List(ref l) => {
                        let value = eval_expr(item, scope);
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
                        let mut inner_scope = captured_scope.clone();

                        if fun.params.len() != arguments.len() {
                            panic!(
                                "Expected {} arguments, found {}",
                                fun.params.len(),
                                arguments.len()
                            );
                        }

                        for (ident, arg) in fun.params.iter().zip(arguments) {
                            let val = eval_expr(arg, scope);
                            inner_scope.locals.insert(ident.clone(), val);
                        }

                        eval_block(&fun.body, inner_scope);

                        // FIXME Return value
                        Arc::new(ScriptValue::Void)
                    }
                }
            }
        },
        // _ => unimplemented!("Expression: {expr:?}"),
    }
}

fn eval_str(src: Arc<str>, scope: &Scope) -> ScriptValue {
    // We probably need the parser to play a role in this.
    // This implementation silently ignores missing references.
    let mut res = src.to_string();
    for (ident, value) in &scope.locals {
        let fmt = format!("${ident}");
        while res.contains(&fmt) {
            let val = match value.as_ref() {
                ScriptValue::String(s) => s.clone(),
                _ => panic!("Unexpected token in string interpolation!"),
            };
            res = res.replace(&fmt, &val);
        }
    }
    ScriptValue::String(res.into())
}
