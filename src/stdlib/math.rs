use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeError, TypeResult},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub(crate) fn build(builder: &mut Builder) {
    builder.add_method(global::INT, "abs", AbsMethod);
}

struct AbsMethod;
impl NativeMethod for AbsMethod {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        match subject {
            ScriptValue::Int(n) => Ok(ScriptValue::Int(n.abs())),
            _ => Err(ScriptError::panic("Not an int")),
        }
    }

    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        match subject {
            ScriptType::Int => Ok(ScriptType::Int),
            _ => Err(TypeError::expected_number(subject.clone())),
        }
    }
}
