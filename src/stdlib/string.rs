use std::sync::Arc;

use crate::{
    error::TypeError,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::{NativeMethod, list::List},
    validate::ScriptType,
};

pub(crate) struct StringLines;
impl NativeMethod for StringLines {
    fn call(&self, _: &Interpreter, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::String(subject) = subject {
            let lines = subject
                .lines()
                .filter(|l| !l.is_empty())
                .map(|l| ScriptValue::String(Arc::from(l)))
                .collect();
            ScriptValue::List(Arc::new(List::new(lines)))
        } else {
            panic!("Not a string")
        }
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Str))
    }
}
