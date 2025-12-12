use std::fs;

use crate::{
    Builder,
    error::ScriptError,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("File::read_all", ReadFunc);
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
        let name = arguments.single()?;

        match name {
            ScriptValue::String(name) => match fs::read_to_string(name.as_ref()) {
                Ok(content) => Ok(ScriptValue::String(content.into())),
                Err(err) => Err(ScriptError::panic(err)),
            },
            _ => Err(ScriptError::panic("Expected a string")),
        }
    }
}
