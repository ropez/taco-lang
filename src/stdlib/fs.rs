use std::fs;

use crate::{
    Builder,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("read", ReadFunc);
    builder.add_function("json", JsonFunc);
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

struct JsonFunc;

impl NativeFunction for JsonFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::List(ScriptType::Str.into()))
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Str
    }

    // We need "json" to be a method, so that it can handle different types of data.
    // Maybe extenstions need to "plug in" to the type system/analyzer.

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptValue {
        let Some(value) = arguments.at(0) else {
            todo!("Return errors from extensions")
        };

        match value {
            ScriptValue::List(list) => {
                let mut s = String::new();

                s.push_str("[");
                let mut iter = list.items().iter();
                if let Some(val) = iter.next() {
                    match val {
                        ScriptValue::String(v) => s.push_str(v),
                        _ => todo!("Serialize {val:?}"),
                    }
                }
                s.push_str("]");

                ScriptValue::String(s.into())
            }
            _ => {
                todo!("Return errors from extensions")
            }
        }
    }
}
