use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    interpreter::{External, Interpreter, ScriptValue, Tuple},
    stdlib::{ExternalValue, Methods, NativeFunction, NativeMethod},
    validate::{ExternalType, ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("State", MakeState::new());
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

struct MakeState {
    methods: Arc<Methods>,
}

impl MakeState {
    fn new() -> Self {
        let mut methods = Methods::default();
        methods.add("get", StateGet);
        methods.add("set", StateSet);
        methods.add("update", StateUpdate);
        let methods = Arc::new(methods);
        Self { methods }
    }
}

impl NativeFunction for MakeState {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Ext(
            ExternalType::new("State", self.methods.clone()).with_inner(ScriptType::Infer(1)),
        )
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single();
        let value = StateValue::new(arg.clone());
        let typ = ExternalType::new("State", self.methods.clone());
        let ext = External::new(Arc::new(typ), Arc::new(value));
        Ok(ScriptValue::Ext(ext))
    }
}

pub(crate) struct StateGet;
impl NativeMethod for StateGet {
    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::Ext(ext) = subject {
            // XXX downcast to some opague struct instead?
            Ok(ext.inner().unwrap().clone())
        } else {
            panic!("Not a state")
        }
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        Ok(subject.as_state().get())
    }
}

pub(crate) struct StateSet;
impl NativeMethod for StateSet {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        if let ScriptType::Ext(ext) = subject {
            Ok(TupleType::from_single(ext.inner().unwrap().clone()))
        } else {
            panic!("Not a state")
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::Ext(ext) = subject {
            Ok(ext.inner().unwrap().clone())
        } else {
            panic!("Not a state")
        }
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let val = arguments.single();
        let state = subject.as_state();
        state.set(val.clone());
        Ok(state.get())
    }
}

pub(crate) struct StateUpdate;
impl NativeMethod for StateUpdate {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        if let ScriptType::Ext(ext) = subject {
            Ok(TupleType::from_single(ScriptType::Function {
                params: TupleType::from_single(ext.inner().unwrap().clone()),
                ret: Box::new(ext.inner().unwrap().clone()),
            }))
        } else {
            panic!("Not a state")
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::Ext(ext) = subject {
            Ok(ext.inner().unwrap().clone())
        } else {
            panic!("Not a state")
        }
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single().clone(); // XXX Maybe we can have owned args here
        let state = subject.as_state();
        state.update(|v| interpreter.eval_callable(callable, &v.to_single_argument()))?;
        Ok(state.get())
    }
}

impl ScriptValue {
    fn as_state(&self) -> &StateValue {
        if let ScriptValue::Ext(ext) = self {
            if let Some(state) = ext.downcast_ref::<StateValue>() {
                state
            } else {
                panic!("Invalid state data");
            }
        } else {
            panic!("Not a state");
        }
    }
}
