use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::{NativeFunction, NativeMethod},
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_global(
        "None",
        ScriptType::opt_of(ScriptType::Unknown),
        ScriptValue::opt(None),
    );
    builder.add_function("Some", SomeFunction);

    builder.add_method(global::OPT, "is_none", IsNoneMethod);
    builder.add_method(global::OPT, "is_some", IsSomeMethod);
}

struct SomeFunction;
impl NativeFunction for SomeFunction {
    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        let arg = arguments.single().cloned()?;
        Ok(TupleType::from_single(arg))
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single().cloned()?;
        Ok(ScriptType::opt_of(arg))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let val = arguments.single()?;
        Ok(ScriptValue::opt(Some(val.clone())))
    }
}

struct IsNoneMethod;
impl NativeMethod for IsNoneMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let opt = subject.as_opt()?;
        Ok(ScriptValue::Boolean(opt.is_none()))
    }
}

struct IsSomeMethod;
impl NativeMethod for IsSomeMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let opt = subject.as_opt()?;
        Ok(ScriptValue::Boolean(opt.is_some()))
    }
}
