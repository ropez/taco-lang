use std::sync::Arc;

use crate::{
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::{Record, TypeExpression},
    stdlib::NativeFunction,
};

pub(crate) struct ParseFunc {
    def: Arc<Record>,
}

impl ParseFunc {
    pub(crate) fn new(def: Arc<Record>) -> Self {
        Self { def }
    }
}

impl NativeFunction for ParseFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptValue {
        if let Some(ScriptValue::String(s)) = arguments.first() {
            let mut values = Vec::new();
            let mut tokens = s.split_ascii_whitespace();
            for d in &self.def.params {
                values.push(TupleItem::new(
                    d.name.clone(),
                    match d.type_expr.as_ref() {
                        TypeExpression::Scalar(ident) => match ident.as_str() {
                            "str" => ScriptValue::String(tokens.next().unwrap().into()),
                            "int" => ScriptValue::Number(tokens.next().unwrap().parse::<i64>().unwrap()),
                            o => panic!("Don't know how to parse {o:?}")
                        }
                        o => panic!("Don't know how to parse {o:?}")
                    }
                ));
            }

            ScriptValue::Rec {
                def: Arc::clone(&self.def),
                value: Arc::new(Tuple::new(values)),
            }
        } else {
            panic!("Expected string, got {arguments:?}");
        }
    }
}
