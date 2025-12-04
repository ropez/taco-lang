use std::sync::Arc;

use crate::{
    error::TypeError, ident::global, interpreter::{Interpreter, ScriptValue, Tuple, TupleItem}, stdlib::{list::List, NativeMethod}, validate::{ScriptType, TupleItemType, TupleType}, Builder
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::STRING, "lines", StringLines);
    builder.add_method(global::STRING, "split_at", StringSplitAt);
}

pub(crate) struct StringLines;
impl NativeMethod for StringLines {
    fn call(&self, _: &Interpreter, subject: &ScriptValue, _arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::String(subject) = subject {
            let lines = subject
                .lines()
                .filter(|l| !l.is_empty())
                .map(|l| ScriptValue::String(Arc::from(l)))
                .collect();
            ScriptValue::List(Arc::new(List::new(lines)))
        } else {
            panic!("Not a string")
        }
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Str))
    }
}

pub(crate) struct StringSplitAt;
impl NativeMethod for StringSplitAt {
    fn call(&self, _: &Interpreter, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue {
        if let ScriptValue::String(subject) = subject {
            let arg = arguments.single().as_number() as usize;
            let (l, r) = subject .split_at(arg);

            let items = [l, r].into_iter().map(Arc::from).map(ScriptValue::String).map(TupleItem::unnamed).collect();
            ScriptValue::Tuple(Tuple::new(items).into())
        } else {
            panic!("Not a string")
        }
    }

    fn arguments_type(&self, _: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        let items = vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::unnamed(ScriptType::Str),
        ];
        Ok(ScriptType::Tuple(TupleType::from(items)))
    }
}
