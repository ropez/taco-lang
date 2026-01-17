use crate::{
    Builder,
    error::{ScriptError, ScriptResult},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_function("panic", PanicFunc);
}

pub(crate) struct PanicFunc;
impl NativeFunction for PanicFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single()?.as_string()?;
        Err(ScriptError::panic(arg))
    }
}
