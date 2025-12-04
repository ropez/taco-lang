use std::sync::{Arc, RwLock};

use crate::{
    Builder,
    error::TypeError,
    ident::global,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::{NativeFunction, NativeMethod},
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("state", MakeState);
    builder.add_method(global::STATE, "get", StateGet);
    builder.add_method(global::STATE, "set", StateSet);
    builder.add_method(global::STATE, "update", StateUpdate);
}

struct MakeState;
impl NativeFunction for MakeState {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Generic(1))
    }
    fn return_type(&self) -> ScriptType {
        ScriptType::State(ScriptType::Generic(1).into())
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptValue {
        let arg = arguments.first().expect("state arg");
        ScriptValue::State(Arc::new(RwLock::new(arg.clone())))
    }
}

pub(crate) struct StateGet;
impl NativeMethod for StateGet {
    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::State(inner) = subject {
            Ok(*inner.clone())
        } else {
            panic!("Not a state")
        }
    }

    fn call(&self, _: &Interpreter, subject: &ScriptValue, _arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::State(state) = subject {
            let v = state.read().unwrap();
            v.clone()
        } else {
            panic!("Not a state")
        }
    }
}

pub(crate) struct StateSet;
impl NativeMethod for StateSet {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        if let ScriptType::State(inner) = subject {
            Ok(TupleType::from_single(*inner.clone()))
        } else {
            panic!("Not a state")
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::State(inner) = subject {
            Ok(*inner.clone())
        } else {
            panic!("Not a state")
        }
    }

    fn call(&self, _: &Interpreter, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::State(state) = subject {
            let val = arguments.single();
            {
                let mut v = state.write().unwrap();
                *v = val.clone();
            }
            state.read().unwrap().clone()
        } else {
            panic!("Not a state")
        }
    }
}

pub(crate) struct StateUpdate;
impl NativeMethod for StateUpdate {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        if let ScriptType::State(inner) = subject {
            Ok(TupleType::from_single(ScriptType::Function {
                params: TupleType::from_single(*inner.clone()),
                ret: Box::new(*inner.clone()),
            }))
        } else {
            panic!("Not a state")
        }
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        if let ScriptType::State(inner) = subject {
            Ok(*inner.clone())
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
        if let ScriptValue::State(state) = subject {
            let callable = arguments.single();
            {
                let mut v = state.write().unwrap();
                let new_val = interpreter.eval_callable(callable, &v.to_single_argument());
                *v = new_val.clone();
            }
            state.read().unwrap().clone()
        } else {
            panic!("Not a state")
        }
    }
}
