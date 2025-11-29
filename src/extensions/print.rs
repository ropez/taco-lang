use std::{
    collections::HashMap,
    io,
    sync::{Arc, Mutex},
};

use crate::{
    eval::{ScriptValue, Tuple},
    extensions::NativeFunction,
    validate::{ScriptType, TupleType},
};

struct PrintFunc<O>
where
    O: io::Write + 'static,
{
    out: Arc<Mutex<O>>,
    newline: bool,
}

impl<O> NativeFunction for PrintFunc<O>
where
    O: io::Write + 'static,
{
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn call(&self, arguments: &Tuple) -> ScriptValue {
        if let Some(arg) = arguments.at(0) {
            let mut out = self.out.lock().unwrap();
            write!(out, "{arg}").unwrap();
            if self.newline {
                writeln!(out).unwrap();
            }
        }
        ScriptValue::identity()
    }
}

pub fn create<O>(out: Arc<Mutex<O>>) -> HashMap<String, Arc<dyn NativeFunction>>
where
    O: io::Write + 'static,
{
    let mut ext: HashMap<String, Arc<dyn NativeFunction>> = HashMap::new();

    ext.insert(
        "print".into(),
        Arc::new(PrintFunc {
            out: Arc::clone(&out),
            newline: false,
        }),
    );

    ext.insert(
        "println".into(),
        Arc::new(PrintFunc {
            out: Arc::clone(&out),
            newline: true,
        }),
    );

    ext
}
