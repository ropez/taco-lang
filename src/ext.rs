use std::{any::Any, collections::HashMap, fmt, ops, sync::Arc};

use crate::{
    error::{ScriptError, TypeError},
    ident::Ident,
    interpreter::{Interpreter, ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub trait ExternalType {
    fn name(&self) -> Ident;
    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef>;

    fn inner(&self) -> Option<&ScriptType> {
        None
    }

    // XXX Try to get rid of this
    fn with_inner(&self, inner: ScriptType) -> Arc<dyn ExternalType + Send + Sync>;
}

impl fmt::Debug for dyn ExternalType + Send + Sync {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[exernal]")
    }
}

pub trait ExternalValue {
    fn as_any(&self) -> &dyn Any;
}

impl fmt::Debug for dyn ExternalValue + Send + Sync {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
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

    fn return_type(&self) -> ScriptType {
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

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = subject;
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
