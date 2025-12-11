use std::collections::HashMap;

use tinyjson::JsonValue;

use crate::{
    Builder,
    error::{ScriptError, ScriptErrorKind},
    interpreter::{Interpreter, ScriptValue, Tuple},
    parser::{Expression, Record},
    stdlib::NativeFunction,
    validate::{ScriptType, TupleType},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("json", JsonFunc);
}

struct JsonFunc;
impl NativeFunction for JsonFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Infer(0))
    }

    fn return_type(&self) -> ScriptType {
        ScriptType::Str
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let jv = JsonValue::from(arguments.single()?);
        match jv.stringify() {
            Ok(json) => Ok(ScriptValue::String(json.into())),
            Err(err) => Err(ScriptError::panic(err)),
        }
    }
}

impl From<&ScriptValue> for JsonValue {
    fn from(value: &ScriptValue) -> Self {
        match value {
            ScriptValue::Int(n) => JsonValue::Number(*n as f64),
            ScriptValue::String(s) => JsonValue::String(s.to_string()),
            ScriptValue::Tuple(value) => JsonValue::from(value.as_ref()),
            ScriptValue::Rec { def, value } => JsonValue::Object(transform_record(def, value)),
            ScriptValue::List(l) => {
                let items: Vec<_> = l.items().iter().map(JsonValue::from).collect();
                JsonValue::Array(items)
            }

            _ => panic!("into json for {value:?}"),
        }
    }
}

fn transform_record(def: &Record, value: &Tuple) -> HashMap<String, JsonValue> {
    let mut items = HashMap::new();

    for (i, (item, d)) in value.items().iter().zip(def.params.as_ref()).enumerate() {
        let default_name = d
            .name
            .as_ref()
            .map(|n| n.to_string())
            .unwrap_or_else(|| i.to_string());
        let json_attr = d
            .attrs
            .iter()
            .map(|it| it.as_ref())
            .find(|it| it.name.as_str() == "json");
        let name = if let Some(attr) = json_attr {
            // XXX Need to "resolve" attribute args like in "transform_args" (interpreter.rs)
            // It means that the params to the "@json" attribute must be defined somewhere, so that
            // we can validate etc. Probably it should just be a function!
            if let Some(args) = &attr.args {
                if let Some(f) = args.first() {
                    if let Expression::Str(s) = f.expr.as_ref() {
                        s.to_string()
                    } else if let Expression::String(t) = f.expr.as_ref() {
                        if let Some(Expression::Str(s)) = t.first().map(|t| t.0.as_ref())
                            && t.len() == 1
                        {
                            s.to_string()
                        } else {
                            panic!("String interpolation in attribute");
                        }
                    } else {
                        panic!(
                            "Expected string expression in @json attr, found {:?}",
                            f.expr
                        );
                    }
                } else {
                    default_name
                }
            } else {
                default_name
            }
        } else {
            default_name
        };
        items.insert(name, JsonValue::from(&item.value));
    }

    items
}

impl From<&Tuple> for JsonValue {
    fn from(value: &Tuple) -> Self {
        let mut map = HashMap::new();

        for (i, item) in value.items().iter().enumerate() {
            let key = item
                .name
                .as_ref()
                .map(|n| n.to_string())
                .unwrap_or_else(|| i.to_string());
            map.insert(key, JsonValue::from(&item.value));
        }

        JsonValue::Object(map)
    }
}
