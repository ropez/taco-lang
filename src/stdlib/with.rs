use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptResult, TypeResult},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ScriptValue, Tuple},
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
    ) -> ScriptResult<ScriptValue> {
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

    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        let formal = match subject {
            ScriptType::Tuple(typ) => typ,
            ScriptType::RecInstance(rec) => &rec.params,
            _ => {
                panic!("Not a rec");
            }
        };

        let mut items = Vec::new();
        for p in formal.items() {
            if let Some(name) = &p.name {
                items.push(TupleItemType::named(name, p.value.as_optional()))
            }
        }

        Ok(TupleType::new(items))
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(subject.clone())
    }
}

fn update_tuple(value: &mut Arc<Tuple>, arguments: &Tuple) {
    // Clone on write, and get a mutable reference to the items
    let values = Arc::make_mut(value).mut_items();

    let mut args = arguments.iter_args();

    for item in values.iter_mut() {
        if let Some(name) = &item.name
            && let Some(val) = args.get(name)
        {
            match &mut item.value {
                ScriptValue::Tuple(tuple) => {
                    update_tuple(tuple, &val.as_tuple().expect("must be a tuple"))
                }
                ScriptValue::Rec { value, .. } => {
                    update_tuple(value, &val.as_tuple().expect("must be a tuple"))
                }
                _ => {
                    item.value = val.clone();
                }
            }
        }
    }
}
