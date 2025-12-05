use std::{any::Any, cell::RefCell, rc::Rc};

use crate::{
    Builder,
    error::TypeError,
    interpreter::{External, Interpreter, ScriptValue, Tuple},
    stdlib::{ExternalValue, NativeFunction, NativeMethod},
    validate::{ExternalType, ScriptType, TupleType},
};

const STATE_NS: &str = "__state__";

pub fn build(builder: &mut Builder) {
    builder.add_function("state", MakeState);
    builder.add_method(STATE_NS, "get", StateGet);
    builder.add_method(STATE_NS, "set", StateSet);
    builder.add_method(STATE_NS, "update", StateUpdate);
}

struct StateValue {
    inner: RefCell<ScriptValue>,
}

impl StateValue {
    fn new(inner: ScriptValue) -> Self {
        Self {
            inner: RefCell::new(inner),
        }
    }

    fn get(&self) -> ScriptValue {
        self.inner.borrow().clone()
    }

    fn set(&self, value: ScriptValue) {
        *self.inner.borrow_mut() = value;
    }

    fn update(&self, f: impl FnOnce(&ScriptValue) -> ScriptValue) {
        let mut val = self.inner.borrow_mut();
        *val = f(&val);
    }
}

impl ExternalValue for StateValue {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct MakeState;
impl NativeFunction for MakeState {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Generic(1))
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Ext(ExternalType::new(STATE_NS, "State").with_inner(ScriptType::Generic(1)))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptValue {
        let arg = arguments.single();
        let value = StateValue::new(arg.clone());
        let ext = External::new(STATE_NS, "State", Rc::new(value));
        ScriptValue::Ext(ext)
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

    fn call(&self, _: &Interpreter, subject: &ScriptValue, _arguments: &Tuple) -> ScriptValue {
        subject.as_state().get()
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

    fn call(&self, _: &Interpreter, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        let val = arguments.single();
        let state = subject.as_state();
        state.set(val.clone());
        state.get()
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
        subject: &ScriptValue,
        arguments: &Tuple,
    ) -> ScriptValue {
        let callable = arguments.single();
        let state = subject.as_state();
        state.update(|v| interpreter.eval_callable(callable, &v.to_single_argument()));
        state.get()
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
