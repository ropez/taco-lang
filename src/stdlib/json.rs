use std::{collections::HashMap, sync::Arc};

use tinyjson::JsonValue;

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    parser::{Expression, Record, TypeExpression},
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

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Str
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let jv = JsonValue::from(arguments.single()?);
        match jv.stringify() {
            Ok(json) => Ok(ScriptValue::string(json)),
            Err(err) => Err(ScriptError::panic(err)),
        }
    }
}

pub(crate) struct ParseJson {
    def: Arc<Record>,
}

impl ParseJson {
    pub(crate) fn new(def: Arc<Record>) -> Self {
        Self { def }
    }
}

impl NativeFunction for ParseJson {
    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let input = arguments.single()?.as_string()?;
        let json: JsonValue = input.parse().map_err(ScriptError::panic)?;
        let obj: &HashMap<_, _> = json
            .get()
            .ok_or_else(|| ScriptError::panic("Expected a JSON object"))?;
        let mut values = Vec::new();
        for d in self.def.params.as_ref() {
            let name = d
                .name
                .as_ref()
                .ok_or_else(|| ScriptError::panic("Can't parse record with unnamed fields"))?;
            let val = obj
                .get(name.as_str())
                .ok_or_else(|| ScriptError::panic(format!("Attribute '{name}' not found")))?;
            values.push(TupleItem::new(
                d.name.clone(),
                match d.type_expr.as_ref() {
                    TypeExpression::Int => {
                        let n: &f64 = val.get().ok_or_else(|| {
                            ScriptError::panic(format!("Expected number, found {val:?}"))
                        })?;
                        ScriptValue::Int(*n as i64)
                    }
                    TypeExpression::Bool => {
                        let n: &bool = val.get().ok_or_else(|| {
                            ScriptError::panic(format!("Expected number, found {val:?}"))
                        })?;
                        ScriptValue::Boolean(*n)
                    }
                    TypeExpression::Str => {
                        let s: &String = val
                            .get()
                            .ok_or_else(|| ScriptError::panic("Expected string"))?;
                        ScriptValue::string(s.clone())
                    }
                    o => todo!("Don't know how to parse {o:?}"),
                },
            ));
        }

        Ok(ScriptValue::Rec {
            def: Arc::clone(&self.def),
            value: Arc::new(Tuple::new(values)),
        })
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
