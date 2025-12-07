use std::{
    io,
    sync::{Arc, Mutex},
};

use crate::{
    Builder,
    error::{ScriptError, ScriptErrorKind},
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + 'static,
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

    builder.add_function("assert", AssertFunc);
}

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

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        if let Some(arg) = arguments.at(0) {
            let mut out = self.out.lock().unwrap();
            write!(out, "{arg}").unwrap();
            if self.newline {
                writeln!(out).unwrap();
            }
        }
        Ok(ScriptValue::identity())
    }
}

// XXX Later must be a build-in concept that prints the expression used
struct AssertFunc;
impl NativeFunction for AssertFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Bool)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        if let ScriptValue::Boolean(true) = arguments.single() {
            Ok(ScriptValue::identity())
        } else {
            Err(ScriptError::new(ScriptErrorKind::AssertionFailed(
                "TODO".into(),
            )))
        }
    }
}
