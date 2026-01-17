use std::{any::Any, sync::Arc};

use async_lock::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::Interpreter,
    script_type::{FunctionType, ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build(builder: &mut Builder) {
    builder.add_function(
        "State",
        MakeState(Arc::new(StateType::new(ScriptType::Infer(1)))),
    );
}

struct StateType {
    inner: ScriptType,
}

impl StateType {
    fn new(inner: ScriptType) -> Self {
        Self { inner }
    }
}

impl ExternalType for StateType {
    fn name(&self) -> Ident {
        "State".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "get" => Some(NativeMethodRef::from(StateGet)),
            "set" => Some(NativeMethodRef::from(StateSet)),
            "update" => Some(NativeMethodRef::from(StateUpdate)),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct StateValue {
    inner: RwLock<ScriptValue>,
}

impl StateValue {
    fn new(inner: ScriptValue) -> Self {
        Self {
            inner: RwLock::new(inner),
        }
    }

    fn get(&self) -> ScriptValue {
        self.lock_read().clone()
    }

    fn set(&self, value: ScriptValue) {
        *self.lock_write() = value;
    }

    fn update(
        &self,
        f: impl FnOnce(&ScriptValue) -> ScriptResult<ScriptValue>,
    ) -> ScriptResult<()> {
        let mut val = self.lock_write();
        *val = f(&val)?;
        Ok(())
    }

    fn lock_read(&self) -> RwLockReadGuard<'_, ScriptValue> {
        #[cfg(not(target_arch = "wasm32"))]
        let lock = self.inner.read_blocking();
        #[cfg(target_arch = "wasm32")]
        let lock = self.inner.try_read().expect("State read lock");

        lock
    }

    fn lock_write(&self) -> RwLockWriteGuard<'_, ScriptValue> {
        #[cfg(not(target_arch = "wasm32"))]
        let lock = self.inner.write_blocking();
        #[cfg(target_arch = "wasm32")]
        let lock = self.inner.try_write().expect("State read lock");

        lock
    }
}

impl ExternalValue for StateValue {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct MakeState(Arc<dyn ExternalType + Sync + Send>);

impl MakeState {
    fn get_type(&self) -> Arc<dyn ExternalType + Sync + Send> {
        Arc::clone(&self.0)
    }
}

impl NativeFunction for MakeState {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single().cloned()?;
        let typ = StateType::new(arg);
        Ok(ScriptType::Ext(Arc::new(typ)))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let value = arguments.single().cloned()?;
        Ok(ScriptValue::Ext(
            self.get_type(), // Dummy type, not used here, only for get_methods
            Arc::new(StateValue::new(value)),
        ))
    }
}

pub(crate) struct StateGet;
impl NativeMethod for StateGet {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        Ok(subject.as_state()?.get())
    }
}

pub(crate) struct StateSet;
impl NativeMethod for StateSet {
    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        let state = subject.as_state()?;
        Ok(TupleType::from_single(state.inner.clone()))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = arguments.single().cloned()?;
        let state = subject.as_state()?;
        state.set(val);
        Ok(state.get())
    }
}

pub(crate) struct StateUpdate;
impl NativeMethod for StateUpdate {
    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        let state = subject.as_state()?;
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(
                TupleType::from_single(state.inner.clone()),
                state.inner.clone(),
            ),
        )))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments.single().cloned()?; // XXX Maybe we can have owned args here
        let state = subject.as_state()?;
        state.update(|v| interpreter.eval_callable(callable, &v.to_single_argument()))?;
        Ok(state.get())
    }
}

impl ScriptType {
    fn as_state(&self) -> TypeResult<&StateType> {
        self.downcast_ext("State")
    }
}

impl ScriptValue {
    fn as_state(&self) -> ScriptResult<&StateValue> {
        self.downcast_ext()
    }
}
