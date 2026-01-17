use std::fmt::{self, Write};

use crate::{
    Builder,
    error::{ScriptResult, TypeResult},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleType},
    script_value::{ScriptValue, Tuple, TupleItem},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("typeof", TypeOfFunc);
}

struct TypeOfFunc;

impl NativeFunction for TypeOfFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single()?;
        Ok(ScriptValue::string(arg.to_type()))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Str)
    }
}

impl ScriptValue {
    fn to_type(&self) -> String {
        match self {
            ScriptValue::String { .. } => "str".into(),
            ScriptValue::Int(_) => "int".into(),
            ScriptValue::Boolean(_) => "bool".into(),
            ScriptValue::List(items) => {
                if let Some(inner) = items.items().first() {
                    format!("[{}]", inner.to_type())
                } else {
                    "[]".into() // XXX Sometinmes we know the type at compile time
                }
            }
            ScriptValue::Tuple(t) => tuple_to_type(t),
            ScriptValue::Rec { def, value } => format!("{}{}", def.name, tuple_to_type(value)),
            ScriptValue::Enum { def, .. } => {
                format!("{}", def.name)
            }
            ScriptValue::ScriptFunction(f) => {
                format!("fun{}: {}", f.function.params, f.function.ret)
            }
            ScriptValue::EnumVariant { def, index } => {
                let variant = def.variants.get(*index).expect("enum variant exists");
                let params = variant.params.as_ref().expect("enum variant has params");
                format!("fun{}: {}", params, def.name)
            }
            _ => todo!("to_type for {self:?}"),
        }
    }
}

fn tuple_to_type(tuple: &Tuple) -> String {
    let mut f = String::new();
    write!(f, "(").unwrap();

    let mut items = tuple.items().iter();
    if let Some(first) = items.next() {
        write_tuple_item(&mut f, first).unwrap();
        for val in items {
            write!(f, ", ").unwrap();
            write_tuple_item(&mut f, val).unwrap();
        }
    }

    write!(f, ")").unwrap();
    f
}

fn write_tuple_item(f: &mut String, o: &TupleItem) -> fmt::Result {
    if let Some(name) = &o.name {
        write!(f, "{name}: ")?;
    }

    write!(f, "{}", o.value.to_type())?;
    Ok(())
}
