use std::{
    any::Any,
    io::{self},
    process::ExitStatus,
    sync::Arc,
};

use async_lock::Mutex;
use async_trait::async_trait;
use smol::{
    prelude::*,
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{
        ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef, Readable,
        Writable,
    },
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + Send + Sync + 'static,
{
    let _ = out;

    builder.add_function("exec", ExecFunc(Arc::new(ProcessType)));
}

struct ProcessType;
impl ExternalType for ProcessType {
    fn name(&self) -> Ident {
        "Process".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "output" => Some(NativeMethodRef::from(OutputMethod)),
            "running" => Some(NativeMethodRef::from(RunningMethod)),
            _ => None,
        }
    }

    fn as_readable(&self) -> Option<ScriptType> {
        // XXX This should depend on whether input is piped or not
        Some(ScriptType::Str)
    }

    fn as_writable(&self) -> Option<ScriptType> {
        // XXX This should depend on whether input is piped or not
        Some(ScriptType::Str)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct Process {
    child: Mutex<Child>,
    stdin: Mutex<Option<ChildStdin>>,
    stdout: Mutex<Option<ChildStdout>>,
}

impl Process {
    fn new(mut child: Child) -> Self {
        let stdin = child.stdin.take();
        let stdout = child.stdout.take();
        Self {
            stdin: Mutex::new(stdin),
            stdout: Mutex::new(stdout),
            child: Mutex::new(child),
        }
    }

    async fn status(&self) -> Result<ExitStatus, ScriptError> {
        let mut child = self.child.lock().await;
        child.status().await.map_err(ScriptError::panic)
    }
}

impl ExternalValue for Process {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        Some(self)
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        Some(self)
    }
}

#[async_trait]
impl Readable for Process {
    async fn read(&self, _: &Interpreter) -> Result<Option<ScriptValue>, ScriptError> {
        let mut guard = self.stdout.lock().await;
        let stdout = guard
            .as_mut()
            .ok_or_else(|| ScriptError::panic("No stdout"))?;

        let mut buf = [0u8; 4096];
        let n = stdout.read(&mut buf).await.map_err(ScriptError::panic)?;
        if n != 0 {
            let s = str::from_utf8(&buf[..n]).map_err(ScriptError::panic)?;
            Ok(Some(ScriptValue::string(s)))
        } else {
            Ok(None)
        }
    }
}

#[async_trait]
impl Writable for Process {
    async fn write(&self, _: &Interpreter, value: ScriptValue) -> Result<(), ScriptError> {
        let arg = value.as_string()?;

        let mut guard = self.stdin.lock().await;
        let stdin = guard
            .as_mut()
            .ok_or_else(|| ScriptError::panic("No stdin"))?;

        stdin
            .write(arg.as_bytes())
            .await
            .map_err(ScriptError::panic)?;

        Ok(())
    }

    async fn close(&self) -> Result<(), ScriptError> {
        let mut child = self.stdin.lock().await;

        // Drops the value inside the option
        child.take();

        Ok(())
    }
}

struct ExecFunc(Arc<dyn ExternalType + Sync + Send>);

impl ExecFunc {
    fn get_type(&self) -> Arc<dyn ExternalType + Sync + Send> {
        Arc::clone(&self.0)
    }
}

impl NativeFunction for ExecFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let mut args = arguments.iter_args();
        let arg = args
            .get("command")
            .ok_or_else(|| ScriptError::panic("No command"))?
            .as_string()?;
        let pass = args
            .get("pass_output")
            .and_then(|val| val.as_boolean().ok())
            .unwrap_or_default();

        // XXX Naive command line parsing. Not supporting quotes or excape chars.
        let mut tokens = arg.split_ascii_whitespace();
        let cmd = tokens
            .next()
            .ok_or_else(|| ScriptError::panic("Command not found"))?;

        let child = Command::new(cmd)
            .args(tokens)
            .stdin(Stdio::piped())
            .stdout(if pass {
                Stdio::inherit()
            } else {
                Stdio::piped()
            })
            .spawn()
            .map_err(ScriptError::panic)?;

        Ok(ScriptValue::Ext(
            self.get_type(),
            Arc::new(Process::new(child)),
        ))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Ext(self.get_type())
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

        smol::block_on(async {
            p.status().await?;

            let mut guard = p.stdout.lock().await;
            let stdout = guard
                .as_mut()
                .ok_or_else(|| ScriptError::panic("No stdout"))?;

            let mut buf = Vec::new();
            let _ = stdout
                .read_to_end(&mut buf)
                .await
                .map_err(ScriptError::panic)?;
            let s = String::from_utf8(buf).map_err(ScriptError::panic)?;
            Ok(ScriptValue::string(s))
        })
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Str)
    }
}

struct RunningMethod;
impl NativeMethod for RunningMethod {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let p = subject.as_process()?;
        let mut child = p.child.lock_blocking();
        let code = child.try_status().map_err(ScriptError::panic)?;
        Ok(ScriptValue::Boolean(code.is_none()))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Bool)
    }
}

impl ScriptValue {
    fn as_process(&self) -> Result<&Process, ScriptError> {
        self.downcast_ext()
    }
}
