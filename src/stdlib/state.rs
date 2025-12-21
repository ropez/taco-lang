use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::{Interpreter, ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
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
        self.inner.read().unwrap().clone()
    }

    fn set(&self, value: ScriptValue) {
        *self.inner.write().unwrap() = value;
    }

    fn update(
        &self,
        f: impl FnOnce(&ScriptValue) -> Result<ScriptValue, ScriptError>,
    ) -> Result<(), ScriptError> {
        let mut val = self.inner.write().unwrap();
        *val = f(&val)?;
        Ok(())
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

    fn return_type(&self, arguments: &TupleType) -> ScriptType {
        let t = arguments
            .single()
            .cloned()
            .unwrap_or(ScriptType::identity());
        let typ = StateType::new(t.clone());
        ScriptType::Ext(Arc::new(typ))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let value = arguments.single()?;
        Ok(ScriptValue::Ext(
            self.get_type(), // Dummy type, not used here, only for get_methods
            Arc::new(StateValue::new(value.clone())),
        ))
    }
}

pub(crate) struct StateGet;
impl NativeMethod for StateGet {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        Ok(subject.as_state()?.get())
    }
}

pub(crate) struct StateSet;
impl NativeMethod for StateSet {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let state = subject.as_state()?;
        Ok(TupleType::from_single(state.inner.clone()))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let val = arguments.single()?;
        let state = subject.as_state()?;
        state.set(val.clone());
        Ok(state.get())
    }
}

pub(crate) struct StateUpdate;
impl NativeMethod for StateUpdate {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let state = subject.as_state()?;
        Ok(TupleType::from_single(ScriptType::Function {
            params: TupleType::from_single(state.inner.clone()),
            ret: Box::new(state.inner.clone()),
        }))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        let state = subject.as_state()?;
        Ok(state.inner.clone())
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single()?.clone(); // XXX Maybe we can have owned args here
        let state = subject.as_state()?;
        state.update(|v| interpreter.eval_callable(callable, &v.to_single_argument()))?;
        Ok(state.get())
    }
}

impl ScriptType {
    fn as_state(&self) -> Result<&StateType, TypeError> {
        self.downcast_ext("State")
    }
}

impl ScriptValue {
    fn as_state(&self) -> Result<&StateValue, ScriptError> {
        self.downcast_ext()
    }
}
