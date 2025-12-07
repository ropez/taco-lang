use std::fmt::{self, Write};

use crate::{
    Builder,
    error::ScriptError,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::{ParamExpression, TypeExpression},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("typeof", TypeOfFunc);
}

struct TypeOfFunc;

impl NativeFunction for TypeOfFunc {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let arg = arguments.single();
        Ok(ScriptValue::String(arg.to_type().into()))
    }

    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(1))
    }

    fn return_type(&self) -> crate::validate::ScriptType {
        ScriptType::Str
    }
}

impl ScriptValue {
    fn to_type(&self) -> String {
        match self {
            ScriptValue::String(_) => "str".into(),
            ScriptValue::Int(_) => "int".into(),
            ScriptValue::List(items) => {
                if let Some(inner) = items.items().first() {
                    format!("[{}]", inner.to_type())
                } else {
                    "[]".into() // XXX Sometinmes we know the type at compile time
                }
            }
            ScriptValue::Tuple(t) => tuple_to_type(t),
            ScriptValue::Enum { def, .. } => {
                format!("{}", def.name)
            }
            ScriptValue::EnumVariant { def, index } => {
                let variant = def.variants.get(*index).expect("enum variant exists");
                let params = variant.params.as_ref().expect("enum variant has params");
                format!("fun{}: {}", params_to_type(params), def.name)
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

fn params_to_type(items: &[ParamExpression]) -> String {
    let mut f = String::new();
    write!(f, "(").unwrap();

    let mut items = items.iter();
    if let Some(first) = items.next() {
        write_tuple_item_2(&mut f, first).unwrap();
        for val in items {
            write!(f, ", ").unwrap();
            write_tuple_item_2(&mut f, val).unwrap();
        }
    }

    write!(f, ")").unwrap();
    f
}

fn write_tuple_item_2(f: &mut String, o: &ParamExpression) -> fmt::Result {
    if let Some(name) = &o.name {
        write!(f, "{name}: ")?;
    }
    write_type_expr(f, o.type_expr.as_ref())?;
    Ok(())
}

fn write_type_expr(f: &mut String, type_expr: &TypeExpression) -> fmt::Result {
    match type_expr {
        TypeExpression::Scalar(ident) => write!(f, "{ident}")?,
        TypeExpression::List(inner) => {
            write!(f, "[")?;
            write_type_expr(f, inner.as_ref().as_ref())?;
            write!(f, "]")?;
        }
        TypeExpression::Opt(inner) => {
            write_type_expr(f, inner.as_ref().as_ref())?;
            write!(f, "?")?;
        }
        TypeExpression::Tuple(param_expressions) => write!(f, "...")?,
    }

    Ok(())
}
