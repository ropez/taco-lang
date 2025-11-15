use std::{io, sync::{Arc, Mutex}};

use crate::{eval::{NativeFn, ScriptValue}, validate::ScriptType};

pub struct ExtensionFunction {
    pub name: Arc<str>,
    pub script_type: ScriptType,
    pub func: Box<dyn NativeFn>,
}

pub fn create<O>(out: Arc<Mutex<O>>) -> Vec<ExtensionFunction>
    where O: io::Write + 'static
{
    let print_type = ScriptType::Function {
        params: vec![("_".into(), ScriptType::Str)],
        ret: Box::new(ScriptType::Void),
    };

    let print_out = out.clone();
    let print_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
            let mut out = print_out.lock().unwrap();
            write!(out, "{arg}").unwrap();
        }
        ScriptValue::Void
    };

    let println_out = out.clone();
    let println_fn = move |args: &[Arc<ScriptValue>]| {
        if let Some(arg) = args.first() {
            let mut out = println_out.lock().unwrap();
            writeln!(out, "{arg}").unwrap();
        }
        ScriptValue::Void
    };

    vec![
        ExtensionFunction {
            name: "print".into(),
            script_type: print_type.clone(),
            func: Box::new(print_fn),
        },

        ExtensionFunction {
            name: "println".into(),
            script_type: print_type.clone(),
            func: Box::new(println_fn),
        },
    ]
}
