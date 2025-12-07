use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ident::global,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    stdlib::{NativeMethod, list::List},
    validate::{ScriptType, TupleItemType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::STRING, "len", StringLength);
    builder.add_method(global::STRING, "lines", StringLines);
    builder.add_method(global::STRING, "split", StringSplit);
    builder.add_method(global::STRING, "split_at", StringSplitAt);
}

pub(crate) struct StringLength;
impl NativeMethod for StringLength {
    fn call(
        &self,
        _: &Interpreter,
        subject: &ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        if let ScriptValue::String(subject) = subject {
            let len = subject.chars().count() as i64;
            Ok(ScriptValue::Int(len))
        } else {
            panic!("Not a string")
        }
    }

    fn return_type(&self, _subject: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Int)
    }
}

pub(crate) struct StringLines;
impl NativeMethod for StringLines {
    fn call(
        &self,
        _: &Interpreter,
        subject: &ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        if let ScriptValue::String(subject) = subject {
            let lines = subject
                .lines()
                .filter(|l| !l.is_empty())
                .map(|l| ScriptValue::String(Arc::from(l)))
                .collect();
            Ok(ScriptValue::List(Arc::new(List::new(lines))))
        } else {
            panic!("Not a string")
        }
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Str))
    }
}

pub(crate) struct StringSplit;
impl NativeMethod for StringSplit {
    fn call(
        &self,
        _: &Interpreter,
        subject: &ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single();
        let ScriptValue::String(arg) = arg else {
            panic!("Expected a string");
        };

        if let ScriptValue::String(subject) = subject {
            let lines = subject
                .split(arg.as_ref())
                .filter(|l| !l.is_empty())
                .map(|l| ScriptValue::String(Arc::from(l)))
                .collect();
            Ok(ScriptValue::List(Arc::new(List::new(lines))))
        } else {
            panic!("Not a string")
        }
    }

    fn arguments_type(&self, _: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Str))
    }
}

pub(crate) struct StringSplitAt;
impl NativeMethod for StringSplitAt {
    fn call(
        &self,
        _: &Interpreter,
        subject: &ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        if let ScriptValue::String(subject) = subject {
            let arg = arguments.single().as_int() as usize;
            let (l, r) = subject.split_at(arg);

            let items = [l, r]
                .into_iter()
                .map(Arc::from)
                .map(ScriptValue::String)
                .map(TupleItem::unnamed)
                .collect();
            Ok(ScriptValue::Tuple(Tuple::new(items).into()))
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
