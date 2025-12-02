use crate::{
    error::TypeError,
    interpreter::{ScriptValue, Tuple},
    validate::{ScriptType, TupleType},
};

pub(crate) mod fs;
pub(crate) mod list;
pub(crate) mod print;
pub(crate) mod state;
pub(crate) mod type_of;
pub(crate) mod parse;

pub trait NativeFunction {
    fn call(&self, arguments: &Tuple) -> ScriptValue; // Later: Result<ScriptValue, ScriptError>

    fn arguments_type(&self) -> TupleType {
        TupleType::identity()
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::identity()
    }
}

pub trait NativeMethod {
    fn call(&self, subject: &ScriptValue, arguments: &Tuple) -> ScriptValue; // Later: Result<ScriptValue, ScriptError>
}

pub trait NativeMethodType {
    fn arguments_type(&self, subject: &ScriptType) -> Result<TupleType, TypeError> {
        let _ = subject;
        Ok(TupleType::identity())
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = subject;
        Ok(ScriptType::identity())
    }
}
