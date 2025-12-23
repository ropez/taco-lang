use std::{
    any::Any,
    fmt, ops,
    sync::Arc,
    task::{Context, Poll},
};

use crate::{
    error::{ScriptError, TypeError},
    ident::Ident,
    interpreter::Interpreter,
    script_value::{ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub trait ExternalType {
    fn name(&self) -> Ident;
    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef>;
    fn as_any(&self) -> &dyn Any;

    fn as_readable(&self) -> Option<ScriptType> {
        None
    }
    fn as_writable(&self) -> Option<ScriptType> {
        None
    }
}

impl fmt::Debug for dyn ExternalType + Send + Sync {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}}}", self.name())
    }
}

pub trait ExternalValue {
    fn as_any(&self) -> &dyn Any;

    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        None
    }
    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        None
    }
}

impl fmt::Debug for dyn ExternalValue + Send + Sync {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = self.type_id();
        write!(f, "[extern {t:?}]")
    }
}

pub trait Readable {
    fn read(
        &self,
        context: &mut Context,
        interpreter: &Interpreter,
    ) -> Poll<Result<Option<ScriptValue>, ScriptError>>;
}

pub trait Writable {
    fn write(
        &self,
        context: &mut Context,
        interpreter: &Interpreter,
        value: ScriptValue,
    ) -> Poll<Result<(), ScriptError>>;
    fn close(&self, context: &mut Context) -> Poll<Result<(), ScriptError>>;
}

pub trait NativeFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError>;

    fn arguments_type(&self) -> TupleType {
        TupleType::identity()
    }

    fn return_type(&self, arguments: &TupleType) -> ScriptType {
        let _ = arguments;
        ScriptType::identity()
    }
}

pub trait NativeMethod {
    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError>;

    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let _ = subject;
        Ok(TupleType::identity())
    }

    fn return_type(
        &self,
        subject: &ScriptType,
        arguments: &TupleType,
    ) -> Result<ScriptType, TypeError> {
        let _ = subject;
        let _ = arguments;
        Ok(ScriptType::identity())
    }
}

#[derive(Clone)]
pub struct NativeFunctionRef(Arc<dyn NativeFunction + Send + Sync>);

impl<T> From<T> for NativeFunctionRef
where
    T: NativeFunction + Send + Sync + 'static,
{
    fn from(func: T) -> Self {
        Self::new(Arc::new(func))
    }
}

impl NativeFunctionRef {
    pub(crate) fn new(value: Arc<dyn NativeFunction + Send + Sync>) -> Self {
        Self(value)
    }
}

impl ops::Deref for NativeFunctionRef {
    type Target = dyn NativeFunction;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Debug for NativeFunctionRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[native function]")
    }
}

impl PartialEq for NativeFunctionRef {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone)]
pub struct NativeMethodRef(Arc<dyn NativeMethod + Send + Sync>);

impl<T> From<T> for NativeMethodRef
where
    T: NativeMethod + Send + Sync + 'static,
{
    fn from(method: T) -> Self {
        Self::new(Arc::new(method))
    }
}

impl NativeMethodRef {
    pub(crate) fn new(inner: Arc<dyn NativeMethod + Send + Sync>) -> Self {
        Self(inner)
    }
}

impl ops::Deref for NativeMethodRef {
    type Target = dyn NativeMethod;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Debug for NativeMethodRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[native method]")
    }
}

impl PartialEq for NativeMethodRef {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

pub(crate) trait ReadableExt {
    fn blocking_read_next(
        &self,
        interpreter: &Interpreter,
    ) -> Result<Option<ScriptValue>, ScriptError>;
}

#[cfg(feature = "pipe")]
impl ReadableExt for &(dyn Readable + Send + Sync) {
    fn blocking_read_next(
        &self,
        interpreter: &Interpreter,
    ) -> Result<Option<ScriptValue>, ScriptError> {
        let i = interpreter.clone();
        smol::block_on(async move {
            let r = smol::future::poll_fn(|ctx| self.read(ctx, &i)).await?;
            Ok(r)
        })
    }
}
