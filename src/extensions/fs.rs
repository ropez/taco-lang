use std::{collections::HashMap, fs};

use crate::{
    eval::{ScriptValue, Tuple},
    extensions::ExtensionFunction,
    validate::{TupleParameter, TupleType, ScriptType},
};

pub fn create() -> HashMap<String, ExtensionFunction> {
    let mut ext = HashMap::new();

    let read_type = ScriptType::Function {
        params: TupleType::from(vec![TupleParameter::unnamed(ScriptType::Str)]),
        ret: Box::new(ScriptType::Str),
    };

    let read_fn = move |arguments: Tuple| {
        let Some(name) = arguments.at(0) else {
            todo!("Return errors from extensions")
        };

        match name.as_ref() {
            ScriptValue::String(name) => match fs::read_to_string(name.as_ref()) {
                Ok(content) => ScriptValue::String(content.into()),
                Err(err) => todo!("Return errors from extensions: {err}"),
            },
            _ => {
                todo!("Return errors from extensions")
            }
        }
    };

    ext.insert(
        "read".into(),
        ExtensionFunction {
            script_type: read_type.clone(),
            func: Box::new(read_fn),
        },
    );

    // We need "json" to be a method, so that it can handle different types of data.
    // Maybe extenstions need to "plug in" to the type system/analyzer.

    let json_type = ScriptType::Function {
        params: TupleType::from(vec![TupleParameter::unnamed(ScriptType::List(
            ScriptType::Str.into(),
        ))]),
        ret: Box::new(ScriptType::Str),
    };

    let json_fn = move |arguments: Tuple| {
        let Some(value) = arguments.at(0) else {
            todo!("Return errors from extensions")
        };

        match value.as_ref() {
            ScriptValue::List(list) => {
                let mut s = String::new();

                s.push_str("[");
                let mut iter = list.iter();
                if let Some(val) = iter.next() {
                    match val.as_ref() {
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
    };

    ext.insert(
        "json".into(),
        ExtensionFunction {
            script_type: json_type.clone(),
            func: Box::new(json_fn),
        },
    );

    ext
}
