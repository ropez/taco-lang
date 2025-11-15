use std::{
    collections::HashMap,
    io,
    sync::{Arc, Mutex},
};

use crate::{
    eval::{NativeFn, ScriptValue},
    validate::ScriptType,
};

// XXX An exception probably needs to be more than just collection of functions

pub struct ExtensionFunction {
    pub script_type: ScriptType,
    pub func: Box<dyn NativeFn>,
}

pub fn create<O>(out: Arc<Mutex<O>>) -> HashMap<String, ExtensionFunction>
where
    O: io::Write + 'static,
{
    let print_type = ScriptType::Function {
        params: vec![("_".into(), ScriptType::Str)],
        ret: Box::new(ScriptType::identity()),
    };

    let mut ext = HashMap::new();

    let print_out = out.clone();
    let print_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
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
    let println_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
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
