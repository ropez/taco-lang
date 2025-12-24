use std::{
    any::Any,
    io::{self},
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
};

use smol::{
    prelude::*,
    process::{Child, Command, Stdio},
    ready,
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

    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        Some(self)
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        Some(self)
    }
}

impl Readable for Process {
    fn read(
        &self,
        ctx: &mut Context,
        _: &Interpreter,
    ) -> Poll<Result<Option<ScriptValue>, ScriptError>> {
        let mut child = self.child.lock().map_err(ScriptError::panic)?;
        let s = child
            .stdout
            .as_mut()
            .ok_or(ScriptError::panic("No stdout"))?;
        let stdout = Pin::new(s);

        let mut buf = [0u8; 1000];
        let n = ready!(stdout.poll_read(ctx, &mut buf)).map_err(ScriptError::panic)?;
        if n != 0 {
            let s = str::from_utf8(&buf[..n]).map_err(ScriptError::panic)?;
            Poll::Ready(Ok(Some(ScriptValue::string(s))))
        } else {
            Poll::Ready(Ok(None))
        }
    }
}

impl Writable for Process {
    fn write(
        &self,
        ctx: &mut Context,
        _: &Interpreter,
        value: ScriptValue,
    ) -> Poll<Result<(), ScriptError>> {
        let arg = value.as_string()?;
        let mut child = self.child.lock().map_err(ScriptError::panic)?;

        let code = child.try_status().map_err(ScriptError::panic)?;
        if let Some(code) = code {
            eprintln!("OOPS, process already exited with code {code}");
        }

        let out = Pin::new(child.stdin.as_mut().ok_or(ScriptError::panic("No stdin"))?);

        ready!(out.poll_write(ctx, arg.as_bytes())).map_err(ScriptError::panic)?;

        Poll::Ready(Ok(()))
    }

    fn close(&self, _: &mut Context) -> Poll<Result<(), ScriptError>> {
        let mut child = self.child.lock().map_err(ScriptError::panic)?;

        child.stdin.take();

        Poll::Ready(Ok(()))
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
        let arg = arguments.single()?.as_string()?;
        let opt = arguments.get_named_item("pass_output");
        let pass = opt
            .and_then(|o| o.value.as_boolean().ok())
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
        let mut child = p.child.lock().map_err(ScriptError::panic)?;

        smol::block_on(async {
            let _exit_code = child.status().await.map_err(ScriptError::panic)?;
            let out = child
                .stdout
                .as_mut()
                .ok_or(ScriptError::panic("No stdout"))?;
            let mut buf = Vec::new();
            let _ = out
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
        let mut child = p.child.lock().map_err(ScriptError::panic)?;
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
