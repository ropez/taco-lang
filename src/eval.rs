use std::{collections::HashMap, sync::Arc};

use crate::parser::{AstNode, Expression, Function};


pub fn eval(ast: &[AstNode]) {
    let mut scope: HashMap<Arc<str>, Box<str>> = HashMap::new();
    let mut scope_functions: HashMap<Arc<str>, Arc<Function>> = HashMap::new();

    for node in ast {
        match node {
            AstNode::Assignment { name, value } => {
                scope.insert(name.clone(), value.clone());
            }
            AstNode::Function { name, fun } => {
                scope_functions.insert(name.clone(), fun.clone());
            }
            AstNode::Call { subject, arguments } => match subject.as_ref() {
                "println" => {
                    for arg in arguments {
                        match arg {
                            Expression::Ref(ident) => {
                                match scope.get(ident) {
                                    None => panic!("Undefined reference: {ident}"),
                                    Some(v) => println!("{v}"),
                                }
                            }
                            Expression::String(s) => println!("{s}"),
                        }
                    }
                }
                _ => {
                    match scope_functions.get(subject) {
                        None => panic!("Undefined function: {subject:?}"),
                        Some(f) => {
                            // Execute block recursively
                            eval(&f.body)
                        },
                    }
                }
            },
        }
    }
}
