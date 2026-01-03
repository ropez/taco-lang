use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{Fallible, ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::FALLIBLE, "is_ok", IsOkMethod);
    builder.add_method(global::FALLIBLE, "is_err", IsErrMethod);
    builder.add_method(global::FALLIBLE, "value", ValueMethod);
    builder.add_method(global::FALLIBLE, "error", ErrorMethod);
}

struct IsOkMethod;
impl NativeMethod for IsOkMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Bool)
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let b = matches!(subject.as_fallible()?, Fallible::Ok(_));
        Ok(ScriptValue::Boolean(b))
    }
}

struct IsErrMethod;
impl NativeMethod for IsErrMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Bool)
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let b = matches!(subject.as_fallible()?, Fallible::Err(_));
        Ok(ScriptValue::Boolean(b))
    }
}

struct ValueMethod;
impl NativeMethod for ValueMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        if let ScriptType::Fallible(_, err) = subject {
            Ok(ScriptType::opt_of(ScriptType::clone(err)))
        } else {
            Err(TypeError::invalid_argument("fallible", subject.clone()))
        }
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        match subject.as_fallible()? {
            Fallible::Ok(v) => Ok(ScriptValue::clone(v)),
            _ => Ok(ScriptValue::None),
        }
    }
}

struct ErrorMethod;
impl NativeMethod for ErrorMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        if let ScriptType::Fallible(_, err) = subject {
            Ok(ScriptType::opt_of(ScriptType::clone(err)))
        } else {
            Err(TypeError::invalid_argument("fallible", subject.clone()))
        }
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        match subject.as_fallible()? {
            Fallible::Err(v) => Ok(ScriptValue::clone(v)),
            _ => Ok(ScriptValue::None),
        }
    }
}
