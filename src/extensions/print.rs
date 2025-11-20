use std::{
    collections::HashMap,
    io,
    sync::{Arc, Mutex},
};

use crate::{
    eval::{ScriptValue, Tuple},
    extensions::ExtensionFunction,
    validate::{TupleParameter, TupleType, ScriptType},
};

pub fn create<O>(out: Arc<Mutex<O>>) -> HashMap<String, ExtensionFunction>
where
    O: io::Write + 'static,
{
    let print_type = ScriptType::Function {
        params: TupleType::from(vec![TupleParameter::unnamed(ScriptType::Str)]),
        ret: Box::new(ScriptType::identity()),
    };

    let mut ext = HashMap::new();

    let print_out = out.clone();
    let print_fn = move |arguments: Tuple| {
        if let Some(arg) = arguments.at(0) {
            let mut out = print_out.lock().unwrap();
            write!(out, "{arg}").unwrap();
        }
        ScriptValue::identity()
    };

    ext.insert(
        "print".into(),
        ExtensionFunction {
            script_type: print_type.clone(),
            func: Box::new(print_fn),
        },
    );

    let println_out = out.clone();
    let println_fn = move |arguments: Tuple| {
        if let Some(arg) = arguments.at(0) {
            let mut out = println_out.lock().unwrap();
            writeln!(out, "{arg}").unwrap();
        }
        ScriptValue::identity()
    };

    ext.insert(
        "println".into(),
        ExtensionFunction {
            script_type: print_type.clone(),
            func: Box::new(println_fn),
        },
    );

    ext
}
