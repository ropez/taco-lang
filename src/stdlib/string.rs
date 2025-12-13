use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::NativeMethod,
    ident::global,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    stdlib::list::List,
    validate::{ScriptType, TupleItemType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::STRING, "len", StringLength);
    builder.add_method(global::STRING, "lines", StringLines);
    builder.add_method(global::STRING, "trim", StringTrim);
    builder.add_method(global::STRING, "split", StringSplit);
    builder.add_method(global::STRING, "split_at", StringSplitAt);
}

pub(crate) struct StringLength;
impl NativeMethod for StringLength {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let subject = subject.as_string()?;
        let len = subject.chars().count() as i64;
        Ok(ScriptValue::Int(len))
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
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let subject = subject.as_string()?;
        let lines = subject
            .lines()
            .filter(|l| !l.is_empty())
            .map(ScriptValue::string)
            .collect();
        Ok(ScriptValue::List(Arc::new(List::new(lines))))
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::list_of(ScriptType::Str))
    }
}

pub(crate) struct StringTrim;
impl NativeMethod for StringTrim {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let s = subject.as_string()?;
        Ok(ScriptValue::string(s.trim()))
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Str)
    }
}

pub(crate) struct StringSplit;
impl NativeMethod for StringSplit {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let subject = subject.as_string()?;
        let arg = arguments.single()?.as_string()?;

        let lines = subject
            .split(arg.as_ref())
            .filter(|l| !l.is_empty())
            .map(ScriptValue::string)
            .collect();
        Ok(ScriptValue::List(Arc::new(List::new(lines))))
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
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let subject = subject.as_string()?;
        let arg = arguments.single()?.as_int()?;

        let arg = arg.clamp(0, subject.chars().count() as i64) as usize;
        let l: String = subject.chars().take(arg).collect();
        let r: String = subject.chars().skip(arg).collect();

        let items = [l, r]
            .into_iter()
            .map(ScriptValue::string)
            .map(TupleItem::unnamed)
            .collect();
        Ok(ScriptValue::Tuple(Tuple::new(items).into()))
    }

    fn arguments_type(&self, _: &ScriptType) -> Result<TupleType, TypeError> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType) -> Result<ScriptType, TypeError> {
        let items = vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::unnamed(ScriptType::Str),
        ];
        Ok(ScriptType::Tuple(TupleType::new(items)))
    }
}
