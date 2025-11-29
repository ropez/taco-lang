use crate::{eval::{ScriptValue, Tuple}, validate::{ScriptType, TupleType}};

pub(crate) mod state;
pub(crate) mod fs;
pub(crate) mod print;

// XXX An extension probably needs to be more than just collection of functions

pub trait NativeFunction {
    fn arguments_type(&self) -> TupleType {
        TupleType::identity()
    }
    fn return_type(&self) -> ScriptType {
        ScriptType::identity()
    }

    fn call(&self, arguments: &Tuple) -> ScriptValue; // Later: Result<ScriptValue, ScriptError>
}

