use std::{any::Any, fmt, ops, sync::Arc};

use async_trait::async_trait;

use crate::{
    error::{ScriptResult, TypeResult},
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
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

#[async_trait]
pub trait Readable {
    async fn read(&self, interpreter: &Interpreter) -> ScriptResult<Option<ScriptValue>>;
}

#[async_trait]
pub trait Writable {
    async fn write(&self, interpreter: &Interpreter, value: ScriptValue) -> ScriptResult<()>;

    async fn close(&self) -> ScriptResult<()>;
}

pub trait NativeFunction {
    fn call(&self, interpreter: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue>;

    fn arguments_type(&self) -> TupleType {
        TupleType::identity()
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let _ = arguments;
        Ok(ScriptType::identity())
    }
}

pub trait NativeMethod {
    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue>;

    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        let _ = subject;
        Ok(TupleType::identity())
    }

    fn return_type(&self, subject: &ScriptType, arguments: &TupleType) -> TypeResult<ScriptType> {
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

#[cfg(feature = "pipe")]
pub(crate) trait ReadableExt {
    fn blocking_read_next(&self, interpreter: &Interpreter) -> ScriptResult<Option<ScriptValue>>;
}

#[cfg(feature = "pipe")]
impl ReadableExt for &(dyn Readable + Send + Sync) {
    fn blocking_read_next(&self, interpreter: &Interpreter) -> ScriptResult<Option<ScriptValue>> {
        let i = interpreter.clone();
        smol::block_on(async move {
            let r = self.read(&i).await?;
            Ok(r)
        })
    }
}
