use std::{
    any::Any,
    io::{self, Read},
    process::{Child, Command, Stdio},
    sync::{Arc, Mutex},
};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::{External, Interpreter, ScriptValue, Tuple},
    validate::{ExternalType, ScriptType, TupleType},
};

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + Send + Sync + 'static,
{
    let _ = out;

    builder.add_function("exec", ExecFunc);
}

struct ProcessType;
impl ExternalType for ProcessType {
    fn name(&self) -> Ident {
        "Process".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "output" => Some(NativeMethodRef::from(OutputMethod)),
            _ => None,
        }
    }

    fn with_inner(&self, _: ScriptType) -> Arc<dyn ExternalType + Send + Sync> {
        unreachable!()
    }
}

struct Process {
    child: Mutex<Child>,
}

impl Process {
    fn new(child: Child) -> Self {
        Self {
            child: Mutex::new(child),
        }
    }
}

impl ExternalValue for Process {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct ExecFunc;
impl NativeFunction for ExecFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?.as_string()?;

        // XXX Naive command line parsing. Not supporting quotes or excape chars.
        let mut tokens = arg.split_ascii_whitespace();
        let cmd = tokens
            .next()
            .ok_or_else(|| ScriptError::panic("Command not found"))?;

        let child = Command::new(cmd)
            .args(tokens)
            .stdout(Stdio::piped())
            .spawn()
            .map_err(ScriptError::panic)?;

        let ext = External::new(Arc::new(ProcessType), Arc::new(Process::new(child)));
        Ok(ScriptValue::Ext(ext))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Ext(Arc::new(ProcessType))
    }
}

struct OutputMethod;
impl NativeMethod for OutputMethod {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let p = subject.as_process()?;
        let mut child = p.child.lock().map_err(ScriptError::panic)?;
        let out = child
            .stdout
            .as_mut()
            .ok_or(ScriptError::panic("No stdout"))?;
        let mut buf = Vec::new();
        let _ = out.read_to_end(&mut buf).map_err(ScriptError::panic)?;
        let s = String::from_utf8(buf).map_err(ScriptError::panic)?;
        Ok(ScriptValue::string(s))
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Str)
    }
}

impl ScriptValue {
    fn as_process(&self) -> Result<&Process, ScriptError> {
        if let ScriptValue::Ext(ext) = self {
            if let Some(state) = ext.downcast_ref::<Process>() {
                Ok(state)
            } else {
                Err(ScriptError::panic("Invalid state data"))
            }
        } else {
            Err(ScriptError::panic("Not a state"))
        }
    }
}
