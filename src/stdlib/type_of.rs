use crate::{
    Builder,
    interpreter::ScriptValue,
    stdlib::NativeFunction,
    validate::{ScriptType, TupleItemType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("typeof".into(), TypeOfFunc);
}

struct TypeOfFunc;

impl NativeFunction for TypeOfFunc {
    fn call(&self, arguments: &crate::interpreter::Tuple) -> crate::interpreter::ScriptValue {
        let arg = arguments.single();
        let s = format!("{}", arg.to_type());
        ScriptValue::String(s.into())
    }

    fn arguments_type(&self) -> crate::validate::TupleType {
        TupleType::from_single(ScriptType::Generic)
    }

    fn return_type(&self) -> crate::validate::ScriptType {
        ScriptType::Str
    }
}

impl ScriptValue {
    fn to_type(&self) -> ScriptType {
        match self {
            ScriptValue::String(_) => ScriptType::Str,
            ScriptValue::Number(_) => ScriptType::Int,
            ScriptValue::List(items) => {
                if let Some(inner) = items.items().first() {
                    ScriptType::list_of(inner.to_type())
                } else {
                    ScriptType::EmptyList // XXX Sometinmes we know the type at compile time
                }
            }
            ScriptValue::Tuple(t) => {
                let items: Vec<_> = t
                    .items()
                    .iter()
                    .map(|it| TupleItemType::new(it.name.clone(), it.value.to_type()))
                    .collect();

                ScriptType::Tuple(TupleType::from(items))
            }
            // ScriptValue::Callable(f) => {
            //     f.
            // }
            _ => todo!("to_type for {self:?}"),
        }
    }
}
