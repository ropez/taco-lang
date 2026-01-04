use std::{collections::HashMap, sync::Arc};

use tinyjson::JsonValue;

use crate::{
    Builder,
    error::ScriptError,
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{RecType, ScriptType, TupleItemType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib::{list::List, parse::ParseError},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("json", JsonFunc);

    // XXX Register parser here, instead of importing it directly from parse mod
}

struct JsonFunc;
impl NativeFunction for JsonFunc {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Unknown)
    }

    fn return_type(&self, _: &TupleType) -> ScriptType {
        ScriptType::Str
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let jv = JsonValue::try_from(arguments.single()?)?;
        match jv.stringify() {
            Ok(json) => Ok(ScriptValue::string_with_type(json, ContentType::Json)),
            Err(err) => Err(ScriptError::panic(err)),
        }
    }
}

pub(crate) fn parse_json(rec: &RecType, input: &str) -> Result<Tuple, ParseError> {
    let json: JsonValue = input.parse().map_err(ParseError::new)?;
    let obj: &HashMap<_, _> = json
        .get()
        .ok_or_else(|| ParseError::new("Expected a JSON object"))?;
    let mut values = Vec::new();
    for (i, d) in rec.params.items().iter().enumerate() {
        let name = get_json_name(i, d).map_err(|_| ParseError::new("Invalid JSON attribute"))?;
        let val = obj
            .get(name.as_str())
            .ok_or_else(|| ParseError::new(format!("Attribute '{name}' not found")))?;
        values.push(TupleItem::new(
            d.name.clone(),
            from_json_value(&d.value, val)?,
        ));
    }

    Ok(Tuple::new(values))
}

pub(crate) fn from_json_value(
    type_expr: &ScriptType,
    val: &JsonValue,
) -> Result<ScriptValue, ParseError> {
    let value = match type_expr {
        ScriptType::Int => {
            let n: &f64 = val
                .get()
                .ok_or_else(|| ParseError::new(format!("Expected number, found {val:?}")))?;
            ScriptValue::Int(*n as i64)
        }
        ScriptType::Bool => {
            let n: &bool = val
                .get()
                .ok_or_else(|| ParseError::new(format!("Expected number, found {val:?}")))?;
            ScriptValue::Boolean(*n)
        }
        ScriptType::Str => {
            let s: &String = val
                .get()
                .ok_or_else(|| ParseError::new("Expected string"))?;
            ScriptValue::string(s.clone())
        }
        ScriptType::List(inner) => {
            let val: &Vec<_> = val
                .get()
                .ok_or_else(|| ParseError::new(format!("Expected array, found {val:?}")))?;
            let items = val
                .iter()
                .map(|v| from_json_value(inner, v))
                .collect::<Result<Vec<_>, ParseError>>()?;
            let list = List::new(items);
            ScriptValue::List(Arc::new(list))
        }
        ScriptType::RecInstance(rec) => {
            let obj: &HashMap<_, _> = val
                .get()
                .ok_or_else(|| ParseError::new("Expected a JSON object"))?;
            let mut values = Vec::new();
            for (i, d) in rec.params.items().iter().enumerate() {
                let name =
                    get_json_name(i, d).map_err(|_| ParseError::new("Invalid JSON attribute"))?;
                let val = obj
                    .get(name.as_str())
                    .ok_or_else(|| ParseError::new(format!("Attribute '{name}' not found")))?;
                values.push(TupleItem::new(
                    d.name.clone(),
                    from_json_value(&d.value, val)?,
                ));
            }

            ScriptValue::Rec {
                def: Arc::clone(rec),
                value: Arc::new(Tuple::new(values)),
            }
        }
        ScriptType::EnumInstance(e) => {
            let s: &String = val
                .get()
                .ok_or_else(|| ParseError::new("Expected string"))?;

            let (i, var) = e
                .find_variant(&s.as_str().into())
                .ok_or_else(|| ParseError::new("Variant not found"))?;

            if var.params.is_some() {
                Err(ParseError::new("Don't know how to parse enum with params"))?;
            }

            ScriptValue::Enum {
                def: Arc::clone(e),
                index: i,
                value: Arc::new(Tuple::identity()),
            }
        }
        o => todo!("Don't know how to parse {o:?}"),
    };

    Ok(value)
}

impl TryFrom<&ScriptValue> for JsonValue {
    type Error = ScriptError;

    fn try_from(value: &ScriptValue) -> Result<Self, Self::Error> {
        let val = match value {
            ScriptValue::Boolean(b) => JsonValue::Boolean(*b),
            ScriptValue::Int(n) => JsonValue::Number(*n as f64),
            ScriptValue::String { content, .. } => JsonValue::String(content.to_string()),
            ScriptValue::Tuple(value) => JsonValue::try_from(value.as_ref())?,
            ScriptValue::Rec { def, value } => JsonValue::Object(transform_record(def, value)?),
            ScriptValue::List(l) => {
                let items: Vec<_> = l.items().iter().map(JsonValue::try_from).collect::<Result<
                    Vec<_>,
                    ScriptError,
                >>(
                )?;
                JsonValue::Array(items)
            }
            ScriptValue::Enum { def, index, value } => {
                if value.is_empty() {
                    JsonValue::String(def.variants[*index].name.to_string())
                } else {
                    unimplemented!("Serialize enum with values");
                }
            }

            _ => {
                return Err(ScriptError::panic(format!(
                    "Value is not JSON serializable: {value}"
                )));
            }
        };

        Ok(val)
    }
}

fn transform_record(
    def: &RecType,
    value: &Tuple,
) -> Result<HashMap<String, JsonValue>, ScriptError> {
    let mut items = HashMap::new();

    for (i, (item, d)) in value.items().iter().zip(def.params.items()).enumerate() {
        let name = get_json_name(i, d)?;
        items.insert(name, JsonValue::try_from(&item.value)?);
    }

    Ok(items)
}

fn get_json_name(i: usize, expr: &TupleItemType) -> Result<String, ScriptError> {
    let default_name = expr
        .name
        .as_ref()
        .map(|n| n.to_string())
        .unwrap_or_else(|| i.to_string());
    let json_attr = expr.attrs.iter().find(|it| it.name.as_str() == "json");

    if let Some(attr) = json_attr {
        if let Some(args) = &attr.args {
            let mut args = args.iter_args();
            if let Some(f) = args.get("name") {
                let s = f.as_string()?;
                Ok(s.to_string())
            } else {
                Ok(default_name)
            }
        } else {
            Ok(default_name)
        }
    } else {
        Ok(default_name)
    }
}

impl TryFrom<&Tuple> for JsonValue {
    type Error = ScriptError;

    fn try_from(value: &Tuple) -> Result<Self, Self::Error> {
        let mut map = HashMap::new();

        for (i, item) in value.items().iter().enumerate() {
            let key = item
                .name
                .as_ref()
                .map(|n| n.to_string())
                .unwrap_or_else(|| i.to_string());
            map.insert(key, JsonValue::try_from(&item.value)?);
        }

        Ok(JsonValue::Object(map))
    }
}
