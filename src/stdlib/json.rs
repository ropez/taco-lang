use std::{collections::HashMap, sync::Arc};

use tinyjson::JsonValue;

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::NativeFunction,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType, UnionType},
    script_value::{ContentType, ScriptValue, Tuple, TupleItem},
    stdlib::{list::List, parse::ParseError},
    type_scope::TypeDefinition,
};

pub fn build(builder: &mut Builder) {
    builder.add_function("json", JsonFunc);

    // XXX Register parser here, instead of importing it directly from parse mod
}

struct JsonFunc;
impl NativeFunction for JsonFunc {
    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        let arg = arguments.single().cloned()?;
        Ok(TupleType::from_single(arg))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let jv = JsonValue::try_from(arguments.single()?)?;
        match jv.stringify() {
            Ok(json) => Ok(ScriptValue::string_with_type(json, ContentType::Json)),
            Err(err) => Err(ScriptError::panic(err)),
        }
    }
}

macro_rules! parse_error {
    ($($arg:tt)*) => {{
        ParseError::new(format!($($arg)*))
    }};
}

macro_rules! parse_bail {
    ($($arg:tt)*) => {{
        return Err(parse_error!($($arg)*));
    }};
}

pub(crate) fn parse_json(typedef: &TypeDefinition, input: &str) -> Result<ScriptValue, ParseError> {
    match typedef {
        TypeDefinition::RecDefinition(def) => {
            let json: JsonValue = input.parse().map_err(ParseError::new)?;
            let values = parse_typed_tuple(&def.params, &json)?;

            Ok(ScriptValue::Rec {
                def: Arc::clone(def),
                value: Arc::new(values),
            })
        }
        TypeDefinition::UnionDefinition(def) => {
            let json: JsonValue = input.parse().map_err(ParseError::new)?;
            parse_union(def, &json)
        }
    }
}

pub(crate) fn from_json_value(
    type_expr: &ScriptType,
    val: &JsonValue,
) -> Result<ScriptValue, ParseError> {
    let value = match type_expr {
        ScriptType::Int => {
            let n: &f64 = val
                .get()
                .ok_or_else(|| parse_error!("Expected number, found {val:?}"))?;
            ScriptValue::Int(*n as i64)
        }
        ScriptType::Bool => {
            let n: &bool = val
                .get()
                .ok_or_else(|| parse_error!("Expected number, found {val:?}"))?;
            ScriptValue::Boolean(*n)
        }
        ScriptType::Str => {
            let s: &String = val.get().ok_or_else(|| parse_error!("Expected string"))?;
            ScriptValue::string(s.clone())
        }
        ScriptType::List(inner) => {
            let val: &Vec<_> = val
                .get()
                .ok_or_else(|| parse_error!("Expected array, found {val:?}"))?;
            let items = val
                .iter()
                .map(|v| from_json_value(inner, v))
                .collect::<Result<Vec<_>, ParseError>>()?;
            let list = List::new(items);
            ScriptValue::List(Arc::new(list))
        }
        ScriptType::RecInstance(rec) => {
            let tuple = parse_typed_tuple(&rec.params, val)?;

            ScriptValue::Rec {
                def: Arc::clone(rec),
                value: Arc::new(tuple),
            }
        }
        ScriptType::UnionInstance(e) => parse_union(e, val)?,
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
            ScriptValue::Tuple(value) => serialize_tuple_values(value)?,
            ScriptValue::Rec { def, value } => serialize_record_values(&def.params, value)?,
            ScriptValue::List(l) => {
                let items: Vec<_> = l.items().iter().map(JsonValue::try_from).collect::<Result<
                    Vec<_>,
                    ScriptError,
                >>(
                )?;
                JsonValue::Array(items)
            }
            ScriptValue::Union { def, index, value } => {
                let variant = &def.variants[*index];
                match UnionRepr::from(def) {
                    UnionRepr::Tagged => match &variant.params {
                        None => JsonValue::String(variant.name.to_string()),
                        Some(params) => {
                            let mut map: HashMap<String, JsonValue> = HashMap::new();
                            map.insert(
                                variant.name.to_string(),
                                serialize_record_values(params, value)?,
                            );
                            JsonValue::Object(map)
                        }
                    },
                    UnionRepr::Untagged => match &variant.params {
                        None => JsonValue::Null,
                        Some(params) => serialize_record_values(params, value)?,
                    },
                }
            }
            ScriptValue::Opt(o) => match o {
                None => JsonValue::Null,
                Some(v) => JsonValue::try_from(v.as_ref())?,
            },

            _ => {
                return Err(ScriptError::panic(format!(
                    "Value is not JSON serializable: {value}"
                )));
            }
        };

        Ok(val)
    }
}

fn serialize_tuple_values(value: &Tuple) -> ScriptResult<JsonValue> {
    if value.items().iter().all(|i| i.name.is_none()) {
        serialize_array_tuple(value)
    } else {
        let mut map = HashMap::new();

        for (i, item) in value.items().iter().enumerate() {
            let name = item
                .name
                .as_ref()
                .map(|n| n.to_string())
                .unwrap_or_else(|| i.to_string());
            map.insert(name, JsonValue::try_from(&item.value)?);
        }

        Ok(JsonValue::Object(map))
    }
}

fn serialize_record_values(params: &TupleType, value: &Tuple) -> ScriptResult<JsonValue> {
    if params
        .items()
        .iter()
        .all(|i| matches!(get_json_name(i), Ok(None)))
    {
        serialize_array_tuple(value)
    } else {
        let mut items = HashMap::new();

        for (i, (item, d)) in value.items().iter().zip(params.items()).enumerate() {
            let name = get_json_name(d)?.unwrap_or_else(|| i.to_string());
            items.insert(name, JsonValue::try_from(&item.value)?);
        }

        Ok(JsonValue::Object(items))
    }
}

fn serialize_array_tuple(value: &Tuple) -> ScriptResult<JsonValue> {
    if value.items().len() == 1 {
        // Can we actually do this for recs in general, or only of untagged unions?
        let val = value.single()?;
        JsonValue::try_from(val)
    } else {
        let mut list = Vec::new();

        for item in value.items().iter() {
            list.push(JsonValue::try_from(&item.value)?);
        }

        Ok(JsonValue::Array(list))
    }
}

fn parse_typed_tuple(params: &TupleType, val: &JsonValue) -> Result<Tuple, ParseError> {
    if params
        .items()
        .iter()
        .all(|i| matches!(get_json_name(i), Ok(None)))
    {
        if params.items().len() == 1 {
            let p = params.single().unwrap();
            let v = from_json_value(p, val)?;
            Ok(Tuple::new(vec![TupleItem::unnamed(v)]))
        } else {
            parse_array_tuple(params, val)
        }
    } else {
        let obj: &HashMap<_, _> = val
            .get()
            .ok_or_else(|| parse_error!("Expected a JSON object"))?;

        let mut values = Vec::new();
        for (i, d) in params.items().iter().enumerate() {
            let name = get_json_name(d)
                .map_err(|_| parse_error!("Invalid JSON attribute"))?
                .unwrap_or_else(|| i.to_string());
            let val = obj
                .get(name.as_str())
                .ok_or_else(|| parse_error!("Attribute '{name}' not found"))?;
            values.push(TupleItem::new(
                d.name.clone(),
                from_json_value(&d.value, val)?,
            ));
        }

        Ok(Tuple::new(values))
    }
}

fn parse_array_tuple(params: &TupleType, val: &JsonValue) -> Result<Tuple, ParseError> {
    let arr: &Vec<_> = val
        .get()
        .ok_or_else(|| parse_error!("Expected a JSON array"))?;

    let mut values = Vec::new();

    // FIXME Assert length, and zip

    for (i, d) in params.items().iter().enumerate() {
        let val = arr
            .get(i)
            .ok_or_else(|| parse_error!("Attribute '{i}' not found"))?;
        values.push(TupleItem::unnamed(from_json_value(&d.value, val)?));
    }

    Ok(Tuple::new(values))
}

fn parse_union(def: &Arc<UnionType>, val: &JsonValue) -> Result<ScriptValue, ParseError> {
    match UnionRepr::from(def) {
        UnionRepr::Tagged => parse_tagged_union(def, val),
        UnionRepr::Untagged => parse_untagged_union(def, val),
    }
}

fn parse_untagged_union(def: &Arc<UnionType>, val: &JsonValue) -> Result<ScriptValue, ParseError> {
    for (i, var) in def.variants.iter().enumerate() {
        if let Some(params) = &var.params {
            if let Ok(t) = parse_typed_tuple(params, val) {
                return Ok(ScriptValue::Union {
                    def: Arc::clone(def),
                    index: i,
                    value: Arc::new(t),
                });
            }
        } else if val.is_null() {
            // null matches empty variants (e.g. Rust None variant of Option)
            // TODO Validate that each variant has different type when using untagged
            return Ok(ScriptValue::Union {
                def: Arc::clone(def),
                index: i,
                value: Arc::new(Tuple::identity()),
            });
        }
    }

    parse_bail!(
        "No matching variant found for {} in {}",
        val.stringify().unwrap_or_default(),
        def.name
    );
}

fn parse_tagged_union(def: &Arc<UnionType>, val: &JsonValue) -> Result<ScriptValue, ParseError> {
    match val {
        JsonValue::String(s) => {
            let Some((i, var)) = def.find_variant(&s.as_str().into()) else {
                parse_bail!("Variant not found");
            };

            if var.params.is_some() {
                parse_bail!("Missing values for variant: {}", s);
            }

            Ok(ScriptValue::Union {
                def: Arc::clone(def),
                index: i,
                value: Arc::new(Tuple::identity()),
            })
        }
        JsonValue::Object(o) => {
            if o.len() != 1 {
                parse_bail!("Expected exactly one variant for: {}", def.name);
            }

            let Some((name, value)) = o.iter().next() else {
                parse_bail!("Expected variant name");
            };

            let Some((i, var)) = def.find_variant(&name.as_str().into()) else {
                parse_bail!("Variant not found: {}", name);
            };

            match &var.params {
                None => {
                    parse_bail!("Unexpected values for variant: {}", name);
                }
                Some(params) => {
                    let tuple = parse_typed_tuple(params, value)?;

                    Ok(ScriptValue::Union {
                        def: Arc::clone(def),
                        index: i,
                        value: Arc::new(tuple),
                    })
                }
            }
        }
        _ => parse_bail!("Unexpected value for union"),
    }
}

fn get_json_name(expr: &TupleItemType) -> ScriptResult<Option<String>> {
    let default_name = expr.name.as_ref().map(|n| n.to_string());
    let json_attr = expr.attrs.iter().find(|it| it.name.as_str() == "json");

    if let Some(attr) = json_attr {
        if let Some(args) = &attr.args {
            let mut args = args.iter_args();
            if let Some(f) = args.get("name") {
                let s = f.as_string()?;
                Ok(Some(s.to_string()))
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

enum UnionRepr {
    Tagged,
    Untagged,
    // TODO (Rust)
    // InternallyTagged(tag)
    // AdjacentlyTagged(tag, content)
}

impl UnionRepr {
    fn from(def: &UnionType) -> Self {
        // TODO Need something like ArgsIterator for attributes!
        let untagged = def
            .attrs
            .iter()
            .find(|a| a.name.as_str() == "json")
            .iter()
            .any(|a| {
                a.args.iter().any(|t| {
                    t.items()
                        .iter()
                        .any(|v| v.name.is_none() && v.value == ScriptValue::string("untagged"))
                })
            });

        if untagged {
            Self::Untagged
        } else {
            Self::Tagged
        }
    }
}
