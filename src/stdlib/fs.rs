use std::{fs, io::ErrorKind};

use crate::{
    Builder,
    error::{ScriptResult, TypeResult},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("File::read_all", ReadFunc);
}

struct ReadFunc;

impl NativeFunction for ReadFunc {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Need to define custom error types like "IO::Error"
        Ok(ScriptType::fallible_of(ScriptType::Str, ScriptType::Str))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let name = arguments.single()?.as_string()?;

        match fs::read_to_string(name.as_ref()) {
            Ok(content) => Ok(ScriptValue::ok(ScriptValue::string(content))),
            Err(err) => {
                let msg = match err.kind() {
                    ErrorKind::NotFound => "File not found",
                    _ => unimplemented!("Error: {err}"),
                };
                Ok(ScriptValue::err(ScriptValue::string(msg)))
            }
        }
    }
}
