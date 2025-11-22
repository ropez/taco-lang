use std::{fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(Rc<str>);

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
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
