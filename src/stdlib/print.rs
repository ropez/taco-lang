use std::{
    io,
    sync::{Arc, Mutex},
};

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::Interpreter,
    script_value::{ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + Send + Sync + 'static,
{
    builder.add_function(
        "print",
        PrintFunc {
            out: Arc::clone(&out),
            newline: false,
        },
    );

    builder.add_function(
        "println",
        PrintFunc {
            out: Arc::clone(&out),
            newline: true,
        },
    );
}

struct PrintFunc<O>
where
    O: io::Write + Send + Sync + 'static,
{
    out: Arc<Mutex<O>>,
    newline: bool,
}

impl<O> NativeFunction for PrintFunc<O>
where
    O: io::Write + Send + Sync + 'static,
{
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        if let Some(arg) = arguments.at(0) {
            let mut out = self.out.lock().unwrap();
            write!(out, "{arg}").unwrap();
            if self.newline {
                writeln!(out).unwrap();
            }
            out.flush().unwrap();
        }
        Ok(ScriptValue::identity())
    }
}
