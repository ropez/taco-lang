use std::sync::Arc;

use crate::{
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::Record,
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
            let mut nums = s.split_whitespace().map(|t| t.parse::<i64>().unwrap());
            let mut values = Vec::new();
            for d in &self.def.params {
                values.push(TupleItem::new(
                    d.name.clone(),
                    ScriptValue::Number(nums.next().unwrap()),
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
