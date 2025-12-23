use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_value::{ScriptValue, Tuple},
    validate::{ScriptType, TupleItemType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::REC, "with", WithMethod);
    builder.add_method(global::TUPLE, "with", WithMethod);
}

pub(crate) struct WithMethod;

impl NativeMethod for WithMethod {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        match subject {
            ScriptValue::Tuple(mut value) => {
                update_tuple(&mut value, arguments);
                Ok(ScriptValue::Tuple(value))
            }
            ScriptValue::Rec { def, mut value } => {
                update_tuple(&mut value, arguments);
                Ok(ScriptValue::Rec { def, value })
            }
            _ => {
                panic!("Not a rec");
            }
        }
    }

    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let formal = match subject {
            ScriptType::Tuple(typ) => typ,
            ScriptType::Rec { params, .. } => params,
            _ => {
                panic!("Not a rec");
            }
        };

        let mut items = Vec::new();
        for p in formal.items() {
            if let Some(name) = &p.name {
                items.push(TupleItemType::named(name.clone(), p.value.as_optional()))
            }
        }

        Ok(TupleType::new(items))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(subject.clone())
    }
}

fn update_tuple(value: &mut Arc<Tuple>, arguments: &Tuple) {
    // Clone on write, and get a mutable reference to the items
    let values = Arc::make_mut(value).mut_items();

    // XXX Kind-of works by accedent, because interpreter is not inserting 'None' for
    // optional arguments

    for item in values.iter_mut() {
        if let Some(name) = &item.name
            && let Some(val) = arguments.get_named_item(name.clone())
        {
            item.value = val.value.clone();
        }
    }
}
