use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeError, TypeResult},
    ext::{NativeFunction, NativeMethod},
    ident::global,
    interpreter::Interpreter,
    script_type::{FunctionType, ScriptType, TupleType},
    script_value::{Fallible, ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_function("Ok", OkFunction);
    builder.add_function("Err", ErrFunction);
    builder.add_method(global::FALLIBLE, "is_ok", IsOkMethod);
    builder.add_method(global::FALLIBLE, "is_err", IsErrMethod);
    builder.add_method(global::FALLIBLE, "value", ValueMethod);
    builder.add_method(global::FALLIBLE, "error", ErrorMethod);
    builder.add_method(global::FALLIBLE, "map_err", MapErrMethod);
}

struct OkFunction;
impl NativeFunction for OkFunction {
    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        let arg = arguments.single().cloned()?;
        Ok(TupleType::from_single(arg))
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single().cloned()?;
        Ok(ScriptType::fallible_of(arg, ScriptType::Unknown))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single().cloned()?;
        Ok(ScriptValue::ok(arg))
    }
}

struct ErrFunction;
impl NativeFunction for ErrFunction {
    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        let arg = arguments.single().cloned()?;
        Ok(TupleType::from_single(arg))
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single().cloned()?;
        Ok(ScriptType::fallible_of(ScriptType::Unknown, arg))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single()?;
        Ok(ScriptValue::err(arg.clone()))
    }
}

struct IsOkMethod;
impl NativeMethod for IsOkMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let b = matches!(subject.as_fallible()?, Fallible::Ok(_));
        Ok(ScriptValue::Boolean(b))
    }
}

struct IsErrMethod;
impl NativeMethod for IsErrMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let b = matches!(subject.as_fallible()?, Fallible::Err(_));
        Ok(ScriptValue::Boolean(b))
    }
}

struct ValueMethod;
impl NativeMethod for ValueMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        if let ScriptType::Fallible(inner, _) = subject {
            Ok(ScriptType::opt_of(ScriptType::clone(inner)))
        } else {
            Err(TypeError::invalid_argument("fallible", subject.clone()))
        }
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let val = match subject.as_fallible()? {
            Fallible::Ok(v) => Some(ScriptValue::clone(v)),
            _ => None,
        };

        Ok(ScriptValue::opt(val))
    }
}

struct ErrorMethod;
impl NativeMethod for ErrorMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        if let ScriptType::Fallible(_, err) = subject {
            Ok(ScriptType::opt_of(ScriptType::clone(err)))
        } else {
            Err(TypeError::invalid_argument("fallible", subject.clone()))
        }
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let err = match subject.as_fallible()? {
            Fallible::Err(v) => Some(ScriptValue::clone(v)),
            _ => None,
        };

        Ok(ScriptValue::opt(err))
    }
}

struct MapErrMethod;
impl NativeMethod for MapErrMethod {
    fn arguments_type(&self, subject: &ScriptType) -> TypeResult<TupleType> {
        let (_, inner) = subject.as_fallible()?;
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(TupleType::from_single(inner.clone()), ScriptType::Infer(1)),
        )))
    }

    fn return_type(&self, subject: &ScriptType, arguments: &TupleType) -> TypeResult<ScriptType> {
        let (v, inner) = subject.as_fallible()?;
        let arg = arguments.single()?;
        let ret = arg.as_callable_ret(&TupleType::from_single(inner.clone()))?;

        Ok(ScriptType::fallible_of(v.clone(), ret))
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let callable = arguments
            .iter_args()
            .next_positional()
            .ok_or_else(ScriptError::expected_argument)?;

        let fallible = subject.as_fallible()?;
        if let Fallible::Err(err) = fallible {
            let value = interpreter.eval_callable(callable.clone(), &err.to_single_argument())?;
            Ok(ScriptValue::err(value))
        } else {
            Ok(subject)
        }
    }
}
