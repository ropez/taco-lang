use std::{collections::HashMap, fs, sync::Arc};

use crate::{eval::ScriptValue, extensions::ExtensionFunction, validate::ScriptType};

pub fn create() -> HashMap<String, ExtensionFunction> {
    let read_type = ScriptType::Function {
        params: vec![("_".into(), ScriptType::Str)],
        ret: Box::new(ScriptType::Str),
    };

    let mut ext = HashMap::new();

    let read_fn = move |args: &[Arc<ScriptValue>]| {
        let Some(name) = args.first() else {
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

    ext
}
