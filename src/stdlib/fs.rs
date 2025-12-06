use std::{fmt::Write, fs};

use crate::{
    Builder,
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

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptValue {
        let Some(name) = arguments.at(0) else {
            todo!("Return errors from extensions")
        };

        match name {
            ScriptValue::String(name) => match fs::read_to_string(name.as_ref()) {
                Ok(content) => ScriptValue::String(content.into()),
                Err(err) => todo!("Return errors from extensions: {err}"),
            },
            _ => {
                todo!("Return errors from extensions")
            }
        }
    }
}
