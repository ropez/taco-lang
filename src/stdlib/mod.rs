use std::{any::Any, collections::HashMap, fmt, ops, sync::Arc};

use crate::{
    error::{ScriptError, TypeError},
    ident::Ident,
    interpreter::{Interpreter, ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub(crate) mod fs;
pub(crate) mod list;

#[cfg(feature = "json")]
pub(crate) mod json;

pub(crate) mod parse;
pub(crate) mod print;
pub(crate) mod record;
pub(crate) mod state;
pub(crate) mod string;
pub(crate) mod type_of;

pub trait ExternalValue {
    fn as_any(&self) -> &dyn Any;
}

impl fmt::Debug for dyn ExternalValue {
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
pub struct NativeFunctionRef(Arc<dyn NativeFunction>);

impl<T> From<T> for NativeFunctionRef
where
    T: NativeFunction + 'static,
{
    fn from(func: T) -> Self {
        Self::new(Arc::new(func))
    }
}

impl NativeFunctionRef {
    pub(crate) fn new(value: Arc<dyn NativeFunction>) -> Self {
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
pub struct NativeMethodRef(Arc<dyn NativeMethod>);

impl<T> From<T> for NativeMethodRef
where
    T: NativeMethod + 'static,
{
    fn from(method: T) -> Self {
        Self::new(Arc::new(method))
    }
}

impl NativeMethodRef {
    pub(crate) fn new(inner: Arc<dyn NativeMethod>) -> Self {
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

#[derive(Debug, Clone, Default)]
pub(crate) struct Methods(HashMap<Ident, NativeMethodRef>);

impl Methods {
    pub(crate) fn add<T>(&mut self, name: impl Into<Ident>, method: T)
    where
        T: NativeMethod + 'static,
    {
        self.0.insert(name.into(), NativeMethodRef::from(method));
    }

    pub(crate) fn get(&self, name: &Ident) -> Option<&NativeMethodRef> {
        self.0.get(name)
    }
}
