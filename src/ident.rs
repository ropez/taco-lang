use std::{fmt::Display, sync::Arc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(Arc<str>);

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub(crate) mod global {
    pub(crate) const REC: &str = "__rec__";
    pub(crate) const LIST: &str = "__list__";
    pub(crate) const RANGE: &str = "__range__";
    pub(crate) const INT: &str = "__int__";
    pub(crate) const STRING: &str = "__string__";
}
