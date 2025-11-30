use crate::{error::TypeError, interpreter::{ScriptValue, Tuple}, validate::{ScriptType, TupleType}};

pub(crate) mod state;
pub(crate) mod fs;
pub(crate) mod print;
pub(crate) mod list;

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

    // XXX Must return Result<ArgumentError>

    fn arguments_type(&self, subject: &ScriptType) -> TupleType {
        let _ = subject;
        TupleType::identity()
    }

    fn return_type(&self, subject: &ScriptType) -> Result<ScriptType, TypeError> {
        let _ = subject;
        Ok(ScriptType::identity())
    }
}

