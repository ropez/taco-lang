use std::{
    io,
    process::Command,
    sync::{Arc, Mutex},
};

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::ScriptValue,
    validate::{ScriptType, TupleType},
};

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + Send + Sync + 'static,
{
    let _ = out;

    builder.add_function("exec", ExecFunc);
}

struct ExecFunc;
impl NativeFunction for ExecFunc {
    fn call(
        &self,
        _: &crate::interpreter::Interpreter,
        arguments: &crate::interpreter::Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?.as_string()?;

        // XXX Naive command line parsing. Not supporting quotes or excape chars.
        let mut tokens = arg.split_ascii_whitespace();
        let cmd = tokens
            .next()
            .ok_or_else(|| ScriptError::panic("Command not found"))?;

        let out = Command::new(cmd)
            .args(tokens)
            .output()
            .map_err(ScriptError::panic)?;

        let s = String::from_utf8(out.stdout).map_err(ScriptError::panic)?;
        Ok(ScriptValue::string(s))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Str
    }
}
