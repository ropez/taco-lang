use std::fs;

use crate::{
    Builder,
    error::{ScriptError, ScriptErrorKind},
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("read", ReadFunc);
}

struct ReadFunc;

impl NativeFunction for ReadFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Str
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let name = arguments.single();

        match name {
            ScriptValue::String(name) => match fs::read_to_string(name.as_ref()) {
                Ok(content) => Ok(ScriptValue::String(content.into())),
                Err(err) => Err(ScriptError::new(ScriptErrorKind::unknown(err.to_string()))),
            },
            _ => {
                todo!("Return errors from extensions")
            }
        }
    }
}
