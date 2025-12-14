use std::fs;

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::{Interpreter, ScriptValue, Tuple},
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

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Str
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let name = arguments.single()?.as_string()?;

        match fs::read_to_string(name.as_ref()) {
            Ok(content) => Ok(ScriptValue::string(content)),
            Err(err) => Err(ScriptError::panic(err)),
        }
    }
}
