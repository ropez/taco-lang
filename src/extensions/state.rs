use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{
    eval::{ScriptValue, Tuple},
    extensions::NativeFunction,
    validate::{ScriptType, TupleType},
};

struct MakeState;

impl NativeFunction for MakeState {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Generic)
    }
    fn return_type(&self) -> ScriptType {
        ScriptType::State(ScriptType::Generic.into())
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
