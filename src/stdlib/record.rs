use std::sync::Arc;

use crate::{
    error::TypeError,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeMethod,
    validate::{ScriptType, TupleItemType, TupleType},
};

pub(crate) struct RecordWithMethod;
impl NativeMethod for RecordWithMethod {
    fn call(&self, _: &Interpreter, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        let ScriptValue::Rec { def, value } = subject else {
            panic!("Not a rec");
        };

        let mut values: Vec<_> = value.items().into();

        // XXX Kind-of works by accedent, because interpreter is not inserting 'None' for
        // optional arguments

        for (param, v) in def.params.iter().zip(values.iter_mut()) {
            if let Some(name) = &param.name
                && let Some(val) = arguments.get_named_item(name)
            {
                v.value = val.value.clone();
            }
        }

        ScriptValue::Rec {
            def: Arc::clone(def),
            value: Arc::new(Tuple::new(values)),
        }
    }

    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let ScriptType::Rec { params, .. } = subject else {
            panic!("Not a rec");
        };

        let mut items = Vec::new();
        for p in params.items() {
            if let Some(name) = &p.name {
                items.push(TupleItemType::named(name.clone(), p.value.as_optional()))
            }
        }

        Ok(TupleType::from(items))
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(subject.clone())
    }
}
