use std::{
    io,
    sync::{Arc, Mutex},
};

use crate::{
    Builder,
    interpreter::{ScriptValue, Tuple},
    stdlib::NativeFunction,
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

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + 'static,
{
    builder.add_function(
        "print".into(),
        PrintFunc {
            out: Arc::clone(&out),
            newline: false,
        },
    );

    builder.add_function(
        "println".into(),
        PrintFunc {
            out: Arc::clone(&out),
            newline: true,
        },
    );
}
