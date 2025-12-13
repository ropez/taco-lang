use crate::{
    Builder,
    error::ScriptError,
    interpreter::{Interpreter, ScriptValue, Tuple},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_function("Opt::is_none", IsNoneFunc);
}

pub(crate) struct IsNoneFunc;
impl NativeFunction for IsNoneFunc {
    fn return_type(&self) -> ScriptType {
        ScriptType::Bool
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(0))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?;
        Ok(ScriptValue::Boolean(arg.is_none()))
    }
}
