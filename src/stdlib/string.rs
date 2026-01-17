use std::sync::Arc;

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib::list::List,
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::STRING, "len", StringLength);
    builder.add_method(global::STRING, "lines", StringLines);
    builder.add_method(global::STRING, "trim", StringTrim);
    builder.add_method(global::STRING, "chars", StringChars);
    builder.add_method(global::STRING, "split", StringSplit);
    builder.add_method(global::STRING, "split_at", StringSplitAt);
    builder.add_method(global::STRING, "as_json", StringAsType(ContentType::Json));
}

pub(crate) struct StringLength;
impl NativeMethod for StringLength {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let subject = subject.as_string()?;
        let len = subject.chars().count() as i64;
        Ok(ScriptValue::Int(len))
    }

    fn return_type(&self, _subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
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
    ) -> ScriptResult<ScriptValue> {
        let subject = subject.as_string()?;
        let lines = subject.lines().map(ScriptValue::string).collect();
        Ok(ScriptValue::List(Arc::new(List::new(lines))))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
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
    ) -> ScriptResult<ScriptValue> {
        let (s, t) = subject.as_string_and_type()?;
        Ok(ScriptValue::string_with_type(s.trim(), t))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Str)
    }
}

pub(crate) struct StringChars;
impl NativeMethod for StringChars {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let subject = subject.as_string()?;
        let chars = subject.chars().map(ScriptValue::Char).collect();
        Ok(ScriptValue::List(Arc::new(List::new(chars))))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::list_of(ScriptType::Char))
    }
}

pub(crate) struct StringSplit;
impl NativeMethod for StringSplit {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let subject = subject.as_string()?;
        let mut args = arguments.iter_args();
        let arg = args
            .next_positional()
            .ok_or_else(|| ScriptError::panic("Missing argument"))?
            .as_string()?;
        let skip_empty = args
            .get("skip_empty")
            .map(|s| s.as_boolean())
            .transpose()?
            .unwrap_or(false);

        let lines = subject
            .split(arg.as_ref())
            .filter(|l| !skip_empty || !l.is_empty())
            .map(ScriptValue::string)
            .collect();
        Ok(ScriptValue::List(Arc::new(List::new(lines))))
    }

    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        let items = vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::named("skip_empty", ScriptType::opt_of(ScriptType::Bool)),
        ];
        Ok(TupleType::new(items))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
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
    ) -> ScriptResult<ScriptValue> {
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

    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        let items = vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::unnamed(ScriptType::Str),
        ];
        Ok(ScriptType::Tuple(TupleType::new(items)))
    }
}

pub(crate) struct StringAsType(ContentType);
impl NativeMethod for StringAsType {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let s = subject.as_string()?;
        Ok(ScriptValue::string_with_type(s, self.0))
    }
}
