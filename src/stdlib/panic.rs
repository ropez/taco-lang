use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::Interpreter,
    script_value::{ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_function("panic", PanicFunc);
}

pub(crate) struct PanicFunc;
impl NativeFunction for PanicFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single()?.as_string()?;
        Err(ScriptError::panic(arg))
    }
}
