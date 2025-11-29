use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{
    error::ArgumentError, eval::{ScriptValue, Tuple}, validate::{ArgumentType, ScriptType}
};

pub trait NativeFunction {
    fn check_arguments(&self, arguments: &[ArgumentType]) -> Result<ScriptType, ArgumentError>;
    fn call(&self, arguments: &Tuple) -> ScriptValue; // Later: Result<ScriptValue, ScriptError>
}

struct MakeState;

impl NativeFunction for MakeState {
    // XXX Needs to be able to pinpoint which argument failed validation!
    // E.g. "expected int got str" in argument X
    fn check_arguments(&self, arguments: &[ArgumentType]) -> Result<ScriptType, ArgumentError> {
        arguments
            .first()
            .map(|a| ScriptType::clone(a.value.as_ref()))
            .map(|inner| ScriptType::State(inner.into()))
            .ok_or_else(|| ArgumentError::MissingArgument(None))
    }

    fn call(&self, arguments: &Tuple) -> ScriptValue {
        let arg = arguments.first().expect("state arg");
        ScriptValue::State(Arc::new(RwLock::new(arg.clone())))
    }
}

pub fn create() -> HashMap<String, Arc<dyn NativeFunction>> {
    let mut ext: HashMap<String, Arc<dyn NativeFunction>> = HashMap::new();

    ext.insert("state".into(), Arc::new(MakeState));

    ext
}
