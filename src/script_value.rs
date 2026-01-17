use std::{fmt, sync::Arc};

use crate::{
    error::{ScriptError, ScriptResult},
    ext::{ExternalType, ExternalValue, NativeFunctionRef, NativeMethodRef},
    fmt::{fmt_inner_list, fmt_tuple},
    ident::Ident,
    interpreter::Scope,
    parser::Function,
    script_type::{EnumType, FunctionType, RecType},
    stdlib::list::List,
};

#[derive(Debug, Clone)]
pub enum ScriptValue {
    Boolean(bool),
    String {
        content: Arc<str>,
        content_type: ContentType,
    },
    Int(i64),
    Char(char),
    Range(i64, i64),
    List(Arc<List>),
    Tuple(Arc<Tuple>),

    NaN,

    // Like Rust 'Option' type.
    Opt(Option<Box<ScriptValue>>),

    // Like Rust 'Result' type.
    Fallible(Fallible),

    // The *instance* of a record. Not the record itself (which is a callable)
    Rec {
        def: Arc<RecType>,
        value: Arc<Tuple>,
    },

    Enum {
        def: Arc<EnumType>,
        index: usize,
        value: Arc<Tuple>,
    },

    ScriptFunction(ScriptFunction),

    Record(Arc<RecType>),
    EnumVariant {
        def: Arc<EnumType>,
        index: usize,
    },

    NativeFunction(NativeFunctionRef),
    NativeMethodBound(NativeMethodRef, Box<ScriptValue>),

    Ext(
        Arc<dyn ExternalType + Send + Sync>,
        Arc<dyn ExternalValue + Send + Sync>,
    ),
}

impl ScriptValue {
    pub fn identity() -> Self {
        Self::Tuple(Arc::new(Tuple::identity()))
    }

    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Self::String {
            content: s.into(),
            content_type: ContentType::Undefined,
        }
    }

    pub fn string_with_type(s: impl Into<Arc<str>>, content_type: ContentType) -> Self {
        Self::String {
            content: s.into(),
            content_type,
        }
    }

    pub fn empty_string() -> Self {
        Self::string("")
    }

    pub fn ok(value: ScriptValue) -> Self {
        Self::Fallible(Fallible::Ok(value.into()))
    }

    pub fn err(error: ScriptValue) -> Self {
        Self::Fallible(Fallible::Err(error.into()))
    }

    pub fn opt(val: Option<ScriptValue>) -> Self {
        Self::Opt(val.map(Box::new))
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, Self::NaN)
    }

    pub(crate) fn is_opt(&self) -> bool {
        matches!(self, Self::Opt(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::Opt(None))
    }

    pub(crate) fn is_fallible(&self) -> bool {
        matches!(self, Self::Fallible(_))
    }

    pub fn as_int(&self) -> ScriptResult<i64> {
        match self {
            Self::Int(num) => Ok(*num),
            _ => Err(ScriptError::panic(format!(
                "Expected integer, found {self}"
            ))),
        }
    }

    pub fn as_char(&self) -> ScriptResult<char> {
        match self {
            Self::Char(ch) => Ok(*ch),
            _ => Err(ScriptError::panic(format!(
                "Expected integer, found {self}"
            ))),
        }
    }

    pub fn as_boolean(&self) -> ScriptResult<bool> {
        match self {
            Self::Boolean(b) => Ok(*b),
            _ => Err(ScriptError::panic(format!(
                "Expected boolean, found {self}"
            ))),
        }
    }

    pub fn as_string(&self) -> ScriptResult<Arc<str>> {
        match self {
            Self::String { content, .. } => Ok(Arc::clone(content)),
            Self::Char(c) => Ok(c.to_string().into()),
            _ => Err(ScriptError::panic(format!("Expected string, found {self}"))),
        }
    }

    pub fn as_string_and_type(&self) -> ScriptResult<(Arc<str>, ContentType)> {
        match self {
            Self::String {
                content,
                content_type,
            } => Ok((Arc::clone(content), *content_type)),
            _ => Err(ScriptError::panic(format!("Expected string, found {self}"))),
        }
    }

    pub fn as_tuple(&self) -> Option<Arc<Tuple>> {
        match self {
            Self::Tuple(tuple) => Some(Arc::clone(tuple)),
            Self::Rec { value, .. } => Some(Arc::clone(value)),
            _ => None,
        }
    }

    pub fn as_iterable(&self) -> Vec<ScriptValue> {
        match self {
            Self::List(list) => Vec::from(list.items()),
            Self::Range(l, r) => (*l..=*r).map(ScriptValue::Int).collect(),
            _ => panic!("Expected iterable, found {self}"),
        }
    }

    pub(crate) fn as_fallible(&self) -> ScriptResult<&Fallible> {
        if let Self::Fallible(v) = self {
            Ok(v)
        } else {
            Err(ScriptError::panic(format!(
                "Expected fallible, found {self}"
            )))
        }
    }

    pub(crate) fn as_opt(&self) -> ScriptResult<Option<&ScriptValue>> {
        if let Self::Opt(v) = self {
            Ok(v.as_ref().map(|v| v.as_ref()))
        } else {
            Err(ScriptError::panic(format!("Expected option, found {self}")))
        }
    }

    pub fn as_ext(&self) -> ScriptResult<Arc<dyn ExternalValue + Send + Sync>> {
        match self {
            Self::Ext(_, ext) => Ok(Arc::clone(ext)),
            _ => Err(ScriptError::panic("Not readable")),
        }
    }

    pub fn to_single_argument(&self) -> Tuple {
        Tuple(vec![TupleItem::unnamed(self.clone())])
    }

    pub fn downcast_ext<T>(&self) -> ScriptResult<&T>
    where
        T: ExternalValue + 'static,
    {
        if let ScriptValue::Ext(_, value) = self {
            value
                .as_any()
                .downcast_ref::<T>()
                .ok_or_else(|| ScriptError::panic("invalid downcast"))
        } else {
            Err(ScriptError::panic("tried to downcast non-ext value"))
        }
    }
}

impl PartialEq for ScriptValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String { content: l, .. }, Self::String { content: r, .. }) => l == r,
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::Char(l), Self::Char(r)) => l == r,
            (Self::Char(c), Self::String { content, .. })
            | (Self::String { content, .. }, Self::Char(c)) => {
                // Comparing by converting char to string might not be the most performant
                content.as_ref() == c.to_string()
            }
            (Self::Range(l1, l2), Self::Range(r1, r2)) => (l1, l2) == (r1, r2),
            (Self::Tuple(l), Self::Tuple(r)) => l == r,
            (
                Self::Enum {
                    def: dl,
                    index: il,
                    value: lv,
                },
                Self::Enum {
                    def: dr,
                    index: ir,
                    value: rv,
                },
            ) => Arc::ptr_eq(dl, dr) && *il == *ir && *lv == *rv,
            (Self::List(l), Self::List(r)) => l.items() == r.items(),
            (Self::Rec { def: ld, value: lv }, Self::Rec { def: rd, value: rv }) => {
                Arc::ptr_eq(ld, rd) && *lv == *rv
            }
            (Self::Opt(l), Self::Opt(r)) => l == r,
            (Self::Opt(Some(l)), r) | (r, Self::Opt(Some(l))) => *l.as_ref() == *r,
            _ => todo!("Equality for {self:?} and {other:?}"),
        }
    }
}

impl fmt::Display for ScriptValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::Opt(v) => match v {
                None => write!(f, "<no value>"),
                Some(v) => write!(f, "{v}"),
            },
            ScriptValue::String { content, .. } => write!(f, "{content}"),
            ScriptValue::Int(n) => write!(f, "{n}"),
            ScriptValue::Char(c) => write!(f, "{c}"),
            ScriptValue::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            ScriptValue::Range(l, r) => write!(f, "{l}..{r}"),
            ScriptValue::Tuple(t) => write!(f, "{t}"),
            ScriptValue::List(list) => {
                write!(f, "[")?;
                fmt_inner_list(f, list.items())?;
                write!(f, "]")?;
                Ok(())
            }
            ScriptValue::Enum {
                def,
                index,
                value: values,
            } => {
                let var = &def.variants[*index];
                write!(f, "{}", var.name)?;
                if !values.is_empty() {
                    write!(f, "{values}")?
                }
                Ok(())
            }
            ScriptValue::Rec {
                def: rec,
                value: values,
            } => {
                write!(f, "{}", rec.name)?;
                write!(f, "{values}")?;
                Ok(())
            }
            ScriptValue::NaN => write!(f, "NaN"),
            ScriptValue::Fallible(v) => match v {
                Fallible::Ok(v) => write!(f, "Ok({v})"),
                Fallible::Err(v) => write!(f, "Err({v})"),
            },
            ScriptValue::Ext(t, _) => write!(f, "{{{}}}", t.name()),
            _ => todo!("Display impl for {self:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Fallible {
    Ok(Box<ScriptValue>),
    Err(Box<ScriptValue>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ContentType {
    Undefined,
    Json,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleItem {
    pub name: Option<Ident>,
    pub value: ScriptValue,
}

impl TupleItem {
    pub fn new(name: Option<Ident>, value: impl Into<ScriptValue>) -> Self {
        Self {
            name,
            value: value.into(),
        }
    }

    pub fn named(name: Ident, value: impl Into<ScriptValue>) -> Self {
        Self::new(Some(name), value)
    }

    pub fn unnamed(value: impl Into<ScriptValue>) -> Self {
        Self::new(None, value)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Tuple(Vec<TupleItem>);

impl Tuple {
    pub const fn identity() -> Self {
        Self(Vec::new())
    }

    pub fn new(items: Vec<TupleItem>) -> Self {
        Self(items)
    }

    pub fn items(&self) -> &[TupleItem] {
        &self.0
    }

    pub fn mut_items(&mut self) -> &mut Vec<TupleItem> {
        &mut self.0
    }

    pub fn positional(&self) -> impl Iterator<Item = &ScriptValue> {
        self.0
            .iter()
            .filter(|arg| arg.name.is_none())
            .map(|arg| &arg.value)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get_named(&self, key: impl Into<Ident>) -> Option<&ScriptValue> {
        let key = key.into();
        self.0
            .iter()
            .find(|i| i.name.as_ref() == Some(&key))
            .map(|i| &i.value)
    }

    pub fn single(&self) -> ScriptResult<&ScriptValue> {
        self.positional()
            .nth(0)
            .ok_or_else(|| ScriptError::panic("Expected argument"))
    }

    pub fn iter_args(&self) -> ArgsIterator<'_, impl Iterator<Item = &ScriptValue>> {
        ArgsIterator::new(self, self.positional())
    }
}

pub struct ArgsIterator<'a, I>
where
    I: Iterator<Item = &'a ScriptValue>,
{
    tuple: &'a Tuple,
    positional: I,
}

impl<'a, I> ArgsIterator<'a, I>
where
    I: Iterator<Item = &'a ScriptValue>,
{
    pub fn new(tuple: &'a Tuple, positional: I) -> Self {
        Self { tuple, positional }
    }

    pub fn get(&mut self, name: impl Into<Ident>) -> Option<ScriptValue> {
        if let Some(arg) = self.tuple.get_named(name) {
            Some(arg.clone())
        } else {
            self.next_positional()
        }
    }

    pub fn resolve(&mut self, name: Option<&Ident>) -> Option<ScriptValue> {
        if let Some(name) = name {
            self.get(name)
        } else {
            self.next_positional()
        }
    }

    pub fn next_positional(&mut self) -> Option<ScriptValue> {
        self.positional.next().cloned()
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}

#[derive(Debug, Clone)]
pub struct ScriptFunction {
    pub(crate) function: Arc<FunctionType>,
    pub(crate) source: Arc<Function>,
    captured_scope: Arc<Scope>, // XXX Kind-of circular
}

impl ScriptFunction {
    pub(crate) fn new(
        function: Arc<FunctionType>,
        source: Arc<Function>,
        captured_scope: Arc<Scope>,
    ) -> Self {
        Self {
            function,
            source,
            captured_scope,
        }
    }

    pub(crate) fn clone_captured_scope(&self) -> Scope {
        Scope::clone(&self.captured_scope)
    }
}
