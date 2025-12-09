use std::{
    error,
    fmt::{self, Write},
    result,
};

use crate::{
    ident::Ident,
    lexer::Loc,
    validate::{ScriptType, TupleType},
};

#[derive(Clone)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub(crate) loc: Option<Loc>,
}

#[derive(Clone)]
pub enum TypeErrorKind {
    InvalidExpression,
    UndefinedReference(Ident),
    UndefinedMethod {
        type_name: Ident,
        method_name: Ident,
    },
    UndefinedVariant {
        type_name: Ident,
        variant_name: Ident,
    },
    UndefinedAttribute {
        subject: ScriptType,
        attr_name: Ident,
    },
    InvalidArgument {
        expected: String,
        actual: ScriptType,
    },
    InvalidArgumentType {
        expected: ScriptType,
        actual: ScriptType,
    },
    MissingArgument {
        name: Option<Ident>,
        expected: TupleType,
        actual: TupleType,
    },
    UnexpectedArgument {
        expected: TupleType,
        actual: TupleType,
    },
    MissingDestructureArgument {
        name: Option<Ident>,
        actual: TupleType,
    },
    UnexpectedDestructureArgument {
        actual: TupleType,
    },
    InvalidDestructure(ScriptType),
    InvalidIterable(ScriptType),
    InvalidOptional(ScriptType),
    InvalidCallable(ScriptType),
    InvalidMapTo(ScriptType),
    InvalidReturnType {
        expected: ScriptType,
        actual: ScriptType,
    },
    TypeNotInferred,
    MissingReturnStatement,
    EmptyList,
    TypeAssertionFailed(String),
}

// Promote ArgumentError to "Error" below

impl TypeError {
    pub fn new(kind: TypeErrorKind) -> Self {
        Self { kind, loc: None }
    }

    pub fn expected_number(actual: ScriptType) -> Self {
        Self::new(TypeErrorKind::InvalidArgumentType {
            expected: ScriptType::Int,
            actual,
        })
    }

    pub fn at(self, loc: Loc) -> Self {
        Self {
            loc: Some(self.loc.unwrap_or(loc)),
            ..self
        }
    }

    pub fn at_offset(self, offset: usize) -> Self {
        Self {
            loc: self.loc.map(|loc| loc.shift_right(offset)),
            ..self
        }
    }

    pub(crate) fn into_source_error(self, source: &str) -> Error {
        let msg = match self.kind {
            TypeErrorKind::UndefinedReference(ident) => format!("Undefined reference: {ident}"),
            TypeErrorKind::UndefinedMethod {
                type_name,
                method_name,
            } => format!("Method not found: '{method_name}' for {type_name}"),
            TypeErrorKind::UndefinedVariant {
                type_name,
                variant_name,
            } => format!("Variant not found: {variant_name} in {type_name}"),
            TypeErrorKind::UndefinedAttribute { subject, attr_name } => {
                format!("Attribute not found: {attr_name} in {subject}")
            }
            TypeErrorKind::InvalidArgument { expected, actual } => {
                format!("Expected '{expected}', found '{actual}'")
            }
            TypeErrorKind::InvalidArgumentType { expected, actual } => {
                format!("Expected '{expected}', found '{actual}'")
            }
            TypeErrorKind::MissingArgument {
                name,
                expected,
                actual,
            } => {
                if let Some(ident) = name {
                    format!("Missing argument '{ident}': Expected '{expected}', found '{actual}'")
                } else {
                    format!("Missing positional argument: Expected '{expected}', found '{actual}'")
                }
            }
            TypeErrorKind::UnexpectedArgument { expected, actual } => {
                format!("Unexpected argument. Expected {expected}, found {actual}")
            }
            TypeErrorKind::MissingDestructureArgument { name, actual } => {
                if let Some(ident) = name {
                    format!("Missing value for '{ident}': Found '{actual}'")
                } else {
                    format!("Missing value: Found '{actual}'")
                }
            }
            TypeErrorKind::UnexpectedDestructureArgument { actual } => {
                format!("Too many values found. Found {actual}")
            }
            TypeErrorKind::InvalidReturnType { expected, actual } => {
                if expected.is_identity() {
                    format!("Unexpected return value: Expected no value, found '{actual}'")
                } else {
                    format!("Incompatible return type: Expected '{expected}', found '{actual}'")
                }
            }
            TypeErrorKind::MissingReturnStatement => "Missing return statement".into(),
            TypeErrorKind::EmptyList => "List is always empty".into(),
            TypeErrorKind::InvalidExpression => "Expected expression".into(),
            TypeErrorKind::InvalidDestructure(actual) => {
                format!("Invalid destructure: Expected a tuple, found '{actual}'")
            }
            TypeErrorKind::InvalidIterable(actual) => {
                format!("Expected iterable, found '{actual}'")
            }
            TypeErrorKind::InvalidOptional(actual) => {
                format!("Expected option type, found '{actual}'")
            }
            TypeErrorKind::InvalidCallable(actual) => {
                format!("Expected a callable, found '{actual}'")
            }
            TypeErrorKind::InvalidMapTo(actual) => {
                format!("Expected a list of tuples, found '{actual}'")
            }
            TypeErrorKind::TypeNotInferred => "Type can not be inferred".into(),
            TypeErrorKind::TypeAssertionFailed(msg) => {
                format!("Assertion failed during static analysis.\n\t{msg}")
            }
        };

        Error::new(msg, source, self.loc.unwrap_or(Loc::start()))
    }
}

#[derive(Clone)]
pub struct ScriptError {
    kind: ScriptErrorKind,
    loc: Option<Loc>,
}

#[derive(Clone)]
pub enum ScriptErrorKind {
    Panic(String),
    AssertionFailed(String),
}

impl ScriptError {
    pub fn new(kind: ScriptErrorKind) -> Self {
        Self { kind, loc: None }
    }

    pub fn panic(error: impl ToString) -> Self {
        Self {
            kind: ScriptErrorKind::Panic(error.to_string()),
            loc: None,
        }
    }

    pub fn at(self, loc: Loc) -> Self {
        // TODO Collect locations into a "stack"?
        Self {
            loc: Some(self.loc.unwrap_or(loc)),
            ..self
        }
    }

    pub(crate) fn into_source_error(self, source: &str) -> Error {
        let msg = match self.kind {
            ScriptErrorKind::Panic(error) => format!("Script panicked: {error}"),
            ScriptErrorKind::AssertionFailed(msg) => format!("Assertion failed: {msg}"),
        };

        Error::new(msg, source, self.loc.unwrap_or(Loc::start()))
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub loc: Loc,
    details: Vec<String>,
}

impl Error {
    pub fn new(message: String, source: &str, loc: Loc) -> Self {
        let details = format_error_details(source, loc).unwrap_or_default();
        Self {
            message,
            loc,
            details,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "\x1b[31m     {}\x1b[0m", self.message)?;
        writeln!(f)?;
        for line in &self.details {
            write!(f, "{}", line)?;
        }

        Ok(())
    }
}

impl error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

fn format_error_details(source: &str, loc: Loc) -> result::Result<Vec<String>, fmt::Error> {
    let mut ret = Vec::new();

    let first_line = source[..loc.start].lines().count();
    let last_line = source[..loc.end].lines().count();

    let mut pos = 0;
    for (n, line) in source.lines().enumerate() {
        if n + 5 < first_line {
            pos = pos + line.len() + 1;
            continue;
        }
        if n >= last_line + 5 {
            break;
        }

        let mut buf = String::new();

        if loc.start <= pos + line.len() && loc.end >= pos {
            write!(buf, "\x1b[33m")?;
            write!(buf, " >")?;
        } else {
            write!(buf, "  ")?;
        }
        write!(buf, "\x1b[30m")?;
        write!(buf, "{:-5} | ", n + 1)?;

        write!(buf, "\x1b[0m")?;
        writeln!(buf, "{line}")?;
        if loc.start >= pos && loc.start <= pos + line.len() && loc.end <= pos + line.len() {
            write!(buf, "\x1b[33m")?;
            write!(buf, "          ")?;
            writeln!(
                buf,
                "{}{} here",
                " ".repeat(loc.start - pos),
                "^".repeat(loc.end - loc.start)
            )?;
            write!(buf, "\x1b[0m")?;
        }

        ret.push(buf);

        pos = pos + line.len() + 1;
    }

    Ok(ret)
}
