use std::{fmt, result, sync::Arc};

use crate::{
    error::ScriptError,
    ext::{ExternalType, ExternalValue, NativeFunctionRef, NativeMethodRef},
    fmt::{fmt_inner_list, fmt_tuple},
    ident::Ident,
    interpreter::Scope,
    parser::{Enumeration, Function, Record},
    stdlib::list::List,
};

type Result<T> = result::Result<T, ScriptError>;

#[derive(Debug, Clone)]
pub enum ScriptValue {
    Boolean(bool),
    String {
        content: Arc<str>,
    },
    Int(i64),
    Range(i64, i64),
    List(Arc<List>),
    Tuple(Arc<Tuple>),

    NaN,

    // The "opt" type is only checked in validation. During evaluation, it's equvalent to it's
    // inner value, or the special 'None' value.
    None,

    // The *instance* of a record. Not the record itself (which is a callable)
    Rec {
        def: Arc<Record>,
        value: Arc<Tuple>,
    },

    Enum {
        def: Arc<Enumeration>,
        index: usize,
        value: Arc<Tuple>,
    },

    ScriptFunction(ScriptFunction),

    Record(Arc<Record>),
    EnumVariant {
        def: Arc<Enumeration>,
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
        Self::String { content: s.into() }
    }

    pub fn empty_string() -> Self {
        Self::string("")
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, Self::NaN)
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn as_int(&self) -> Result<i64> {
        match self {
            Self::Int(num) => Ok(*num),
            _ => Err(ScriptError::panic(format!(
                "Expected integer, found {self}"
            ))),
        }
    }

    pub fn as_boolean(&self) -> Result<bool> {
        match self {
            Self::Boolean(b) => Ok(*b),
            _ => Err(ScriptError::panic(format!(
                "Expected boolean, found {self}"
            ))),
        }
    }

    pub fn as_string(&self) -> Result<Arc<str>> {
        match self {
            Self::String { content } => Ok(Arc::clone(content)),
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

    pub fn as_ext(&self) -> Result<Arc<dyn ExternalValue + Send + Sync>> {
        match self {
            Self::Ext(_, ext) => Ok(Arc::clone(ext)),
            _ => Err(ScriptError::panic("Not readable")),
        }
    }

    pub fn to_single_argument(&self) -> Tuple {
        Tuple(vec![TupleItem::unnamed(self.clone())])
    }

    pub fn downcast_ext<T>(&self) -> Result<&T>
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
            (Self::String { content: l }, Self::String { content: r }) => l == r,
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
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
            (Self::None, Self::None) => true,
            (Self::None, _) | (_, Self::None) => false,
            _ => todo!("Equality for {self:?}"),
        }
    }
}

impl fmt::Display for ScriptValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScriptValue::None => write!(f, "<no value>"),
            ScriptValue::String { content } => write!(f, "{content}"),
            ScriptValue::Int(n) => write!(f, "{n}"),
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
            ScriptValue::Ext(t, _) => write!(f, "{{{}}}", t.name()),
            _ => todo!("Display impl for {self:?}"),
        }
    }
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

    pub fn first(&self) -> Option<&ScriptValue> {
        self.0.first().map(|item| &item.value)
    }

    pub fn get_named_item(&self, key: impl Into<Ident>) -> Option<&TupleItem> {
        let key = key.into();
        self.0.iter().find(|i| i.name.as_ref() == Some(&key))
    }

    pub fn at(&self, index: usize) -> Option<&ScriptValue> {
        self.0.get(index).map(|item| &item.value)
    }

    pub fn single(&self) -> Result<&ScriptValue> {
        self.0
            .iter()
            .find(|i| i.name.is_none())
            .map(|item| &item.value)
            .ok_or_else(|| ScriptError::panic("Expected argument"))
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}

#[derive(Debug, Clone)]
pub struct ScriptFunction {
    pub(crate) function: Arc<Function>,
    captured_scope: Arc<Scope>, // XXX Kind-of circular
}

impl ScriptFunction {
    pub fn new(function: Arc<Function>, captured_scope: Arc<Scope>) -> Self {
        Self {
            function,
            captured_scope,
        }
    }

    pub(crate) fn clone_captured_scope(&self) -> Scope {
        Scope::clone(&self.captured_scope)
    }
}
