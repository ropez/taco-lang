use std::{
    error,
    fmt::{self, Write},
    result,
};

use crate::{
    ident::Ident,
    lexer::{Loc, Src},
    validate::{ScriptType, TupleType},
};

#[derive(Clone)]
pub enum TypeError {
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
    TypeNotInverred,
    MissingReturnStatement,
    EmptyList,
}

// Promote ArgumentError to "Error" below

impl TypeError {
    pub fn expected_number(actual: ScriptType) -> Self {
        Self::InvalidArgumentType {
            expected: ScriptType::Int,
            actual,
        }
    }

    pub fn at(self, loc: Loc) -> Src<Self> {
        Src::new(self, loc)
    }

    pub(crate) fn into_source_error(self, source: &str, loc: Loc) -> Error {
        let msg = match self {
            TypeError::UndefinedReference(ident) => format!("Undefined reference: {ident}"),
            TypeError::UndefinedMethod {
                type_name,
                method_name,
            } => format!("Method not found: '{method_name}' for {type_name}"),
            TypeError::UndefinedVariant {
                type_name,
                variant_name,
            } => format!("Variant not found: {variant_name} in {type_name}"),
            TypeError::UndefinedAttribute { subject, attr_name } => {
                format!("Attribute not found: {attr_name} in {subject}")
            }
            TypeError::InvalidArgumentType { expected, actual } => {
                format!("Expected '{expected}', found '{actual}'")
            }
            TypeError::MissingArgument {
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
            TypeError::UnexpectedArgument { expected, actual } => {
                format!("Unexpected argument. Expected {expected}, found {actual}")
            }
            TypeError::MissingDestructureArgument { name, actual } => {
                if let Some(ident) = name {
                    format!("Missing value for '{ident}': Found '{actual}'")
                } else {
                    format!("Missing value: Found '{actual}'")
                }
            }
            TypeError::UnexpectedDestructureArgument { actual } => {
                format!("Too many values found. Found {actual}")
            }
            TypeError::InvalidReturnType { expected, actual } => {
                if expected.is_identity() {
                    format!("Unexpected return value: Expected no value, found '{actual}'")
                } else {
                    format!("Incompatible return type: Expected '{expected}', found '{actual}'")
                }
            }
            TypeError::MissingReturnStatement => "Missing return statement".into(),
            TypeError::EmptyList => "List is always empty".into(),
            TypeError::InvalidExpression => "Expected expression".into(),
            TypeError::InvalidDestructure(actual) => {
                format!("Invalid destructure: Expected a tuple, found '{actual}'")
            }
            TypeError::InvalidIterable(actual) => format!("Expected iterable, found '{actual}'"),
            TypeError::InvalidOptional(actual) => format!("Expected option type, found '{actual}'"),
            TypeError::InvalidCallable(actual) => format!("Expected a callable, found '{actual}'"),
            TypeError::InvalidMapTo(actual) => {
                format!("Expected a list of tuples, found '{actual}'")
            }
            TypeError::TypeNotInverred => "Type can not be inferred".into(),
        };

        Error::new(msg, source, loc)
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
