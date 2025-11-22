use crate::{eval::NativeFn, validate::ScriptType};

pub(crate) mod fs;
pub(crate) mod print;

// XXX An extension probably needs to be more than just collection of functions

pub struct ExtensionFunction {
    pub script_type: ScriptType,
    pub func: Box<dyn NativeFn>,
}
