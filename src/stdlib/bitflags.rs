use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::NativeMethod,
    ident::global,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build(builder: &mut Builder) {
    builder.add_method(global::INT, "has_bit", HasBitMethod);
    builder.add_method(global::INT, "set_bit", SetBitMethod);
    builder.add_method(global::INT, "clear_bit", ClearBitMethod);
}

struct HasBitMethod;
impl NativeMethod for HasBitMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Bool)
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = subject.as_int()?;
        let arg = arguments.single()?.as_int()?;
        Ok(ScriptValue::Boolean(val & checked_shift(arg)? != 0))
    }
}

struct SetBitMethod;
impl NativeMethod for SetBitMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = subject.as_int()?;
        let arg = arguments.single()?.as_int()?;
        Ok(ScriptValue::Int(val | checked_shift(arg)?))
    }
}

struct ClearBitMethod;
impl NativeMethod for ClearBitMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Int))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let val = subject.as_int()?;
        let arg = arguments.single()?.as_int()?;
        Ok(ScriptValue::Int(val & !checked_shift(arg)?))
    }
}

fn checked_shift(num: i64) -> ScriptResult<i64> {
    let num: u32 = num.try_into().map_err(ScriptError::panic)?;
    let p = 1i64
        .checked_shl(num)
        .ok_or_else(|| ScriptError::panic("Out of range"))?;

    Ok(p)
}
