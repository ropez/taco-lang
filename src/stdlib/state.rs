use std::{cell::RefCell, rc::Rc};

use crate::{
    Builder,
    error::TypeError,
    interpreter::{External, Interpreter, ScriptValue, Tuple},
    stdlib::{NativeFunction, NativeMethod},
    validate::{ExternalType, ScriptType, TupleType},
};

const STATE_NS: &str = "__state__";

pub fn build(builder: &mut Builder) {
    builder.add_function("state", MakeState);
    builder.add_method(STATE_NS, "get", StateGet);
    builder.add_method(STATE_NS, "set", StateSet);
    builder.add_method(STATE_NS, "update", StateUpdate);
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
        let arg = arguments.first().expect("state arg");

        let ext = External::new(STATE_NS, "State").with_data(Rc::new(RefCell::new(arg.clone())));
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
        if let ScriptValue::Ext(ext) = subject {
            if let Some(state) = ext.downcast_ref::<RefCell<ScriptValue>>() {
                state.borrow().clone()
            } else {
                panic!("Invalid state data")
            }
        } else {
            panic!("Not a state")
        }
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
        if let ScriptValue::Ext(ext) = subject {
            let val = arguments.single();
            if let Some(state) = ext.downcast_ref::<RefCell<ScriptValue>>() {
                {
                    let mut v = state.borrow_mut();
                    *v = val.clone();
                }
                state.borrow().clone()
            } else {
                panic!("Invalid state data")
            }
        } else {
            panic!("Not a state")
        }
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
        if let ScriptValue::Ext(ext) = subject {
            let callable = arguments.single();
            if let Some(state) = ext.downcast_ref::<RefCell<ScriptValue>>() {
                {
                    let mut v = state.borrow_mut();
                    let new_val = interpreter.eval_callable(callable, &v.to_single_argument());
                    *v = new_val.clone();
                }
                state.borrow().clone()
            } else {
                panic!("Invalid state data")
            }
        } else {
            panic!("Not a state")
        }
    }
}
