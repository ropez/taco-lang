use std::{fmt, result, sync::Arc};

use crate::{
    error::{TypeError, TypeErrorKind},
    ext::{ExternalType, NativeFunctionRef, NativeMethodRef},
    fmt::fmt_tuple,
    ident::Ident,
    lexer::Src,
    parser::{Literal, MatchPattern},
    script_value::Tuple,
};

type Result<T> = result::Result<T, TypeError>;

#[derive(Debug, Clone)]
pub enum ScriptType {
    Bool,
    Int,
    Range,
    Str,
    EmptyList,
    Opt(Box<ScriptType>),
    Fallible(Box<ScriptType>, Box<ScriptType>),
    List(Box<ScriptType>),
    Tuple(TupleType),
    RecInstance(Arc<RecType>),
    EnumInstance(Arc<EnumType>),
    EnumVariant {
        params: TupleType,
        def: Arc<EnumType>,
    },
    Function(Arc<FunctionType>),

    NativeFunction(NativeFunctionRef),
    NativeMethodBound(NativeMethodRef, Box<ScriptType>),

    // Special type used to represent an inferred inner type, e.g. state(x: ?): [?] is represented as
    // Function ( params: (Infer(1)), ret: List<Infer(1)> }
    Infer(u16),

    // User internally to represent when a type is not yet known. E.g. an expression that yields an
    // error of type "E" can have a type of Fallible<Unknown, E>. The type must eventually be
    // resolved to a known type, otherwise it's an error.
    Unknown,

    Ext(Arc<dyn ExternalType + Send + Sync>),
}

impl ScriptType {
    // Use () to represent nothing, like Rust
    pub const fn identity() -> Self {
        Self::Tuple(TupleType::identity())
    }

    pub fn list_of(inner: ScriptType) -> Self {
        Self::List(inner.into())
    }

    pub fn opt_of(inner: ScriptType) -> ScriptType {
        Self::Opt(inner.into())
    }

    pub fn fallible_of(inner_value: ScriptType, inner_error: ScriptType) -> ScriptType {
        Self::Fallible(inner_value.into(), inner_error.into())
    }

    pub fn accepts(&self, other: &ScriptType) -> bool {
        match (self, other) {
            (ScriptType::Ext(_), _) => false,
            (ScriptType::Infer(_), _) => true,
            (ScriptType::Unknown, _) => true,
            (ScriptType::Int, ScriptType::Int) => true,
            (ScriptType::Str, ScriptType::Str) => true,
            (ScriptType::Bool, ScriptType::Bool) => true,
            (ScriptType::Range, ScriptType::Range) => true,
            (ScriptType::List(_), ScriptType::EmptyList) => true,
            (ScriptType::List(l), ScriptType::List(r)) => l.accepts(r),
            (ScriptType::List(l), ScriptType::Range) => l.accepts(&ScriptType::Int),
            (ScriptType::Tuple(l), ScriptType::Tuple(r)) => l.accepts(r),
            (ScriptType::Tuple(l), ScriptType::RecInstance(rec)) => l.accepts(&rec.params),
            (ScriptType::EnumInstance(l), ScriptType::EnumInstance(r)) => Arc::ptr_eq(l, r),
            (ScriptType::EnumInstance(_), ScriptType::EnumVariant { .. }) => false,
            (ScriptType::RecInstance(l), ScriptType::RecInstance(r)) => Arc::ptr_eq(l, r),
            (ScriptType::Function(l), rhs) => {
                if let Ok(rp) = rhs.as_callable_params()
                    && let Ok(rr) = rhs.as_callable_ret(&l.params)
                {
                    rp.accepts(&l.params) && l.ret.accepts(&rr)
                } else {
                    false
                }
            }

            // If a function returns str?, it can also optionally call another function that
            // returns str?. This doesn't turn into "str??", it's always just a value nor no value.
            // You can't have a situation where an expression like "if x in opt" sets 'x' to
            // another opt!
            (ScriptType::Opt(l), r) => l.accepts(r.flatten()),
            (ScriptType::Fallible(vl, el), ScriptType::Fallible(vr, er)) => {
                vl.accepts(vr) && el.accepts(er)
            }

            // Allows returning the value directly from a fallible function without wrapping in Ok()
            (ScriptType::Fallible(vl, _), r) => vl.accepts(r),

            // This is somewhat questionable, but I haven't found another solution.
            // An unknown type can be passed to anything. Consider making this more limited,
            // i.e. only allowing (Opt(_), Opt(Unknown)) etc
            (_, ScriptType::Infer(_)) => true,
            (_, ScriptType::Unknown) => true,

            _ => false,
        }
    }

    pub fn flatten(&self) -> &Self {
        if let ScriptType::Opt(opt) = self {
            opt.flatten()
        } else {
            self
        }
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, ScriptType::Opt(_))
    }

    pub fn is_fallible(&self) -> bool {
        matches!(self, ScriptType::Fallible(_, _))
    }

    pub(crate) fn is_exhausted_by(&self, patterns: &[&Src<MatchPattern>]) -> bool {
        for pat in patterns {
            match pat.as_ref() {
                MatchPattern::Discard => return true,
                MatchPattern::Assignee(_) if !self.is_optional() => return true,
                _ => {}
            }
        }

        // Finite types
        match self {
            Self::Bool => {
                if [
                    MatchPattern::Literal(Literal::False),
                    MatchPattern::Literal(Literal::True),
                ]
                .iter()
                .all(|k| patterns.iter().any(|p| k.matches(p)))
                {
                    return true;
                }
            }
            Self::Fallible(_, _) => {
                // This is obviously not a general solution, but it's sufficient as long as we're
                // not supporting pattern-matching on inner values, and we're preventing duplicates
                // and illegal patterns.
                if patterns.len() == 2 {
                    return true;
                }
            }
            Self::EnumInstance(def) => {
                // This is obviously not a general solution, but it's sufficient as long as we're
                // not supporting pattern-matching on inner values, and we're preventing duplicates
                // and illegal patterns.
                if patterns.len() == def.variants.len() {
                    return true;
                }
            }
            _ => {}
        }

        false
    }

    pub fn as_optional(&self) -> ScriptType {
        if let ScriptType::Opt(_) = self {
            self.clone()
        } else {
            ScriptType::Opt(self.clone().into())
        }
    }

    pub fn as_tuple(&self) -> Option<&TupleType> {
        match &self {
            ScriptType::Tuple(tuple) => Some(tuple),
            ScriptType::RecInstance(rec) => Some(&rec.params),
            _ => None,
        }
    }

    pub fn as_readable(&self) -> Option<ScriptType> {
        if let Self::Ext(ext) = self {
            ext.as_readable()
        } else {
            todo!("Called as_readable on {self}, expected an extension type")
        }
    }

    pub fn as_writable(&self) -> Option<ScriptType> {
        if let Self::Ext(ext) = self {
            ext.as_writable()
        } else {
            todo!("Called as_writable on {self}, expected an extension type")
        }
    }

    pub fn as_fallible(&self) -> Result<(&Self, &Self)> {
        if let Self::Fallible(inner_value, inner_type) = self {
            Ok((inner_value, inner_type))
        } else {
            Err(TypeError::invalid_argument("fallible", self.clone()))
        }
    }

    pub fn as_callable_params(&self) -> Result<TupleType> {
        // XXX Too much cloning
        match &self {
            ScriptType::Function(fun) => Ok(fun.params.clone()),
            ScriptType::EnumVariant { params, .. } => Ok(params.clone()),
            ScriptType::NativeFunction(func) => {
                let params = func.arguments_type();
                Ok(params.clone())
            }
            ScriptType::NativeMethodBound(method, subject_typ) => {
                let params = method.arguments_type(subject_typ)?;
                Ok(params.clone())
            }
            _ => Err(TypeError::new(TypeErrorKind::InvalidCallable(self.clone()))),
        }
    }

    pub fn as_callable_ret(&self, arguments: &TupleType) -> Result<ScriptType> {
        match &self {
            ScriptType::Function(fun) => Ok(ScriptType::clone(&fun.ret)),
            ScriptType::EnumVariant { def, .. } => Ok(ScriptType::EnumInstance(Arc::clone(def))),
            ScriptType::NativeFunction(func) => Ok(func.return_type(arguments)),
            ScriptType::NativeMethodBound(method, subject_typ) => {
                Ok(method.return_type(subject_typ, arguments)?)
            }
            _ => Err(TypeError::new(TypeErrorKind::InvalidCallable(self.clone()))),
        }
    }

    pub(crate) fn is_identity(&self) -> bool {
        if let Self::Tuple(t) = self
            && t.items().is_empty()
        {
            true
        } else {
            false
        }
    }

    pub fn downcast_ext<T>(&self, expexted: impl Into<String>) -> Result<&T>
    where
        T: ExternalType + 'static,
    {
        if let ScriptType::Ext(value) = self {
            value.as_any().downcast_ref::<T>().ok_or_else(|| {
                TypeError::new(TypeErrorKind::InvalidArgument {
                    expected: expexted.into(),
                    actual: self.clone(),
                })
            })
        } else {
            Err(TypeError::new(TypeErrorKind::InvalidArgument {
                expected: expexted.into(),
                actual: self.clone(),
            }))
        }
    }
}

impl fmt::Display for ScriptType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Str => write!(f, "str"),
            Self::EnumInstance(typ) => write!(f, "{}", typ.name),
            Self::EnumVariant { def, params } => {
                write!(f, "fun{params}: {}", def.name)
            }
            Self::EmptyList => write!(f, "[]"),
            Self::List(inner) => write!(f, "[{inner}]"),
            Self::Opt(inner) => write!(f, "{inner}?"),
            Self::Fallible(v, e) => write!(f, "{v} ~ {e}"),
            Self::Tuple(arguments) => write!(f, "{arguments}"),
            Self::RecInstance(rec) => write!(f, "{}{}", rec.name, rec.params),
            Self::Function(fun) => write!(f, "fun{}: {}", fun.params, fun.ret),
            Self::Infer(n) => write!(f, "<{n}>"),
            Self::Unknown => write!(f, "{{unknown}}"),
            Self::Ext(e) => write!(f, "{{{}}}", e.name()),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug)]
pub struct FunctionType {
    pub params: TupleType,
    pub ret: ScriptType,
}

impl FunctionType {
    pub fn new(params: TupleType, ret: ScriptType) -> Arc<Self> {
        Arc::new(Self { params, ret })
    }
}

#[derive(Debug)]
pub struct RecType {
    pub(crate) name: Ident,
    pub(crate) params: TupleType,
}

impl RecType {
    pub fn new(name: impl Into<Ident>, params: impl Into<TupleType>) -> Arc<Self> {
        Arc::new(Self {
            name: name.into(),
            params: params.into(),
        })
    }
}

#[derive(Debug)]
pub struct EnumType {
    pub(crate) name: Ident,
    pub(crate) variants: Vec<EnumVariantType>,
}

impl EnumType {
    pub fn new(name: impl Into<Ident>, variants: impl Into<Vec<EnumVariantType>>) -> Arc<Self> {
        Arc::new(Self {
            name: name.into(),
            variants: variants.into(),
        })
    }

    pub fn find_variant(&self, name: &Ident) -> Option<(usize, &EnumVariantType)> {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, v)| v.name == *name)
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariantType {
    pub(crate) name: Ident,
    pub(crate) params: Option<TupleType>,
}

impl EnumVariantType {
    pub fn new(name: impl Into<Ident>, params: Option<TupleType>) -> Self {
        Self {
            name: name.into(),
            params,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TupleItemType {
    pub name: Option<Ident>,
    pub value: ScriptType,
    pub attrs: Vec<TypeAttribute>,
}

impl TupleItemType {
    pub fn new(name: Option<Ident>, value: ScriptType) -> Self {
        Self {
            name,
            value,
            attrs: Vec::new(),
        }
    }

    pub fn new_with_attrs(
        name: Option<Ident>,
        value: ScriptType,
        attrs: Vec<TypeAttribute>,
    ) -> Self {
        Self { name, value, attrs }
    }

    pub fn named(name: impl Into<Ident>, value: ScriptType) -> Self {
        Self::new(Some(name.into()), value)
    }

    pub fn unnamed(value: ScriptType) -> Self {
        Self::new(None, value)
    }

    pub fn optional(name: impl Into<Ident>, value: ScriptType) -> Self {
        Self::new(Some(name.into()), ScriptType::Opt(value.into()))
    }

    pub fn is_optional(&self) -> bool {
        matches!(self.value, ScriptType::Opt(_))
    }
}

#[derive(Debug, Clone)]
pub struct TypeAttribute {
    pub(crate) name: Ident,

    // Refers to Tuple, which is a *value* (not a type), Because these expressions are
    // evaluated statically, and the result of evaluation (value) becomes part of the type.
    pub(crate) args: Option<Tuple>,
}

#[derive(Default, Debug, Clone)]
pub struct TupleType(Vec<TupleItemType>);

impl TupleType {
    pub const fn identity() -> Self {
        Self(Vec::new())
    }

    pub fn new(args: Vec<TupleItemType>) -> Self {
        TupleType(args)
    }

    pub fn from_single(item: ScriptType) -> Self {
        Self(vec![TupleItemType::unnamed(item)])
    }

    pub fn items(&self) -> &[TupleItemType] {
        &self.0
    }

    pub fn positional(&self) -> impl Iterator<Item = &ScriptType> {
        self.0
            .iter()
            .filter(|it| it.name.is_none())
            .map(|it| &it.value)
    }

    pub fn at_pos(&self, n: usize) -> Result<&ScriptType> {
        self.positional().nth(n).ok_or_else(|| {
            TypeError::new(TypeErrorKind::MissingArgument {
                name: None,
                expected: TupleType::identity(), // XXX
                actual: self.clone(),
            })
        })
    }

    pub fn single(&self) -> Result<&ScriptType> {
        self.at_pos(0)
    }

    pub fn get_named(&self, name: impl Into<Ident>) -> Option<&ScriptType> {
        let key = name.into();
        self.items()
            .iter()
            .find(|a| a.name.as_ref() == Some(&key))
            .map(|a| &a.value)
    }

    fn accepts(&self, other: &TupleType) -> bool {
        // Exact same algorithm as validate_args, but returning boolean
        // instead of Result with code reference.

        let mut positional = other.positional();
        for par in self.0.iter() {
            let opt_arg = if let Some(name) = &par.name {
                other.get_named(name).or_else(|| positional.next())
            } else {
                positional.next()
            };

            if let Some(arg) = opt_arg {
                if !par.value.accepts(arg) {
                    return false;
                }
            } else {
                return false;
            }
        }

        // All positional must be consumed
        positional.next().is_none()
    }
}

impl fmt::Display for TupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_tuple(f, self.0.iter().map(|a| (a.name.clone(), &a.value)))
    }
}
