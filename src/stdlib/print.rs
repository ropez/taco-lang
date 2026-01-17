use std::{io, sync::Arc};

use async_lock::Mutex;

use crate::{
    Builder,
    error::ScriptResult,
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
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

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single()?;

        #[cfg(target_arch = "wasm32")]
        let mut out = self.out.try_lock().expect("Print lock");
        #[cfg(not(target_arch = "wasm32"))]
        let mut out = self.out.lock_blocking();

        write!(out, "{arg}").unwrap();
        if self.newline {
            writeln!(out).unwrap();
        }
        out.flush().unwrap();

        Ok(ScriptValue::identity())
    }
}
