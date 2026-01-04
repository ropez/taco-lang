use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{NativeFunction, NativeMethod},
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{Fallible, ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_function("Ok", OkFunction);
    builder.add_function("Err", ErrFunction);
    builder.add_method(global::FALLIBLE, "is_ok", IsOkMethod);
    builder.add_method(global::FALLIBLE, "is_err", IsErrMethod);
    builder.add_method(global::FALLIBLE, "value", ValueMethod);
    builder.add_method(global::FALLIBLE, "error", ErrorMethod);
}

struct OkFunction;
impl NativeFunction for OkFunction {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::fallible_of(ScriptType::Infer(1), ScriptType::Unknown)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?;
        Ok(ScriptValue::ok(arg.clone()))
    }
}

struct ErrFunction;
impl NativeFunction for ErrFunction {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::fallible_of(ScriptType::Unknown, ScriptType::Infer(1))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?;
        Ok(ScriptValue::err(arg.clone()))
    }
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
        if let ScriptType::Fallible(inner, _) = subject {
            Ok(ScriptType::opt_of(ScriptType::clone(inner)))
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
        let val = match subject.as_fallible()? {
            Fallible::Ok(v) => Some(ScriptValue::clone(v)),
            _ => None,
        };

        Ok(ScriptValue::opt(val))
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
        let err = match subject.as_fallible()? {
            Fallible::Err(v) => Some(ScriptValue::clone(v)),
            _ => None,
        };

        Ok(ScriptValue::opt(err))
    }
}
