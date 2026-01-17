use std::{
    error,
    fmt::{self, Write},
    result,
};

use crate::{
    ident::Ident,
    lexer::{Loc, TokenKind},
    parser::MatchPattern,
    script_type::{ScriptType, TupleType},
    script_value::ScriptValue,
};

#[derive(Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub(crate) loc: Option<Loc>,
}

#[derive(Clone)]
pub enum ParseErrorKind {
    UnexpectedToken,
    UnexpectedEndOfInput,
    Expected(String),
    ExpectedKind(TokenKind),
    InvalidNumber,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind) -> Self {
        Self { kind, loc: None }
    }

    pub fn unexpected_token() -> Self {
        Self::new(ParseErrorKind::UnexpectedToken)
    }

    pub fn expected(what: impl Into<String>) -> Self {
        Self::new(ParseErrorKind::Expected(what.into()))
    }

    pub fn expected_kind(kind: TokenKind) -> Self {
        Self::new(ParseErrorKind::ExpectedKind(kind))
    }

    pub fn at(self, loc: impl Into<Option<Loc>>) -> Self {
        Self {
            loc: self.loc.or(loc.into()),
            ..self
        }
    }

    pub fn shift_right(self, offset: usize) -> Self {
        let loc = self.loc.unwrap_or(Loc::start()).shift_right(offset);
        self.at(loc)
    }

    pub(crate) fn into_source_error(self, source: &str) -> Error {
        let msg = match self.kind {
            ParseErrorKind::UnexpectedToken => String::from("Unexpected token"),
            ParseErrorKind::UnexpectedEndOfInput => String::from("Unexpected end of input"),
            ParseErrorKind::Expected(what) => format!("Expected {what}"),
            ParseErrorKind::ExpectedKind(kind) => format!("Expected to find {kind:?} here"),
            ParseErrorKind::InvalidNumber => String::from("Invalid numeric literal"),
        };

        Error::new(msg, source, self.loc.unwrap_or(Loc::start()))
    }
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub(crate) loc: Option<Loc>,
}

#[derive(Clone, Debug)]
pub enum TypeErrorKind {
    InvalidExpression,
    InvalidStaticExpression,
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
    IncompatiblePipe {
        lhs: ScriptType,
        rhs: ScriptType,
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
    InvalidQuestion(ScriptType),
    InvalidCallable(ScriptType),
    InvalidMapTo(ScriptType),
    InvalidReturnType {
        expected: ScriptType,
        actual: ScriptType,
    },
    TryNotAllowed,
    TypeNotInferred,
    MissingReturnStatement,
    EmptyList,
    TypeAssertionFailed(String),
    PatternAlreadyExhausted(MatchPattern),
    PatternNotExhausted(ScriptType),
    MatchHasNoArms,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}") // XXX Display properly
    }
}

// Promote ArgumentError to "Error" below

impl TypeError {
    pub fn new(kind: TypeErrorKind) -> Self {
        Self { kind, loc: None }
    }

    pub fn invalid_argument(expected: impl Into<String>, actual: ScriptType) -> Self {
        Self::new(TypeErrorKind::InvalidArgument {
            expected: expected.into(),
            actual,
        })
    }

    pub fn expected_type(expected: ScriptType, actual: ScriptType) -> Self {
        Self::new(TypeErrorKind::InvalidArgumentType { expected, actual })
    }

    pub fn expected_number(actual: ScriptType) -> Self {
        Self::expected_type(ScriptType::Int, actual)
    }

    pub fn expected_bool(actual: ScriptType) -> Self {
        Self::expected_type(ScriptType::Bool, actual)
    }

    pub fn at(self, loc: impl Into<Option<Loc>>) -> Self {
        Self {
            loc: self.loc.or(loc.into()),
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
                format!("Attribute not found: '{attr_name}' in {subject}")
            }
            TypeErrorKind::InvalidArgument { expected, actual } => {
                format!("Expected {expected}, found '{actual}'")
            }
            TypeErrorKind::InvalidArgumentType { expected, actual } => {
                format!("Expected '{expected}', found '{actual}'")
            }
            TypeErrorKind::IncompatiblePipe { lhs, rhs } => {
                format!("Incompatible pipe '{lhs}' to '{rhs}'")
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
            TypeErrorKind::InvalidStaticExpression => {
                "Expression can not be evaluated statically".into()
            }
            TypeErrorKind::InvalidDestructure(actual) => {
                format!("Invalid destructure: Expected a tuple, found '{actual}'")
            }
            TypeErrorKind::InvalidIterable(actual) => {
                format!("Expected iterable, found '{actual}'")
            }
            TypeErrorKind::InvalidQuestion(actual) => {
                format!("Expected fallible or option type, found '{actual}'")
            }
            TypeErrorKind::InvalidCallable(actual) => {
                format!("Expected a callable, found '{actual}'")
            }
            TypeErrorKind::InvalidMapTo(actual) => {
                format!("Expected a list of tuples, found '{actual}'")
            }
            TypeErrorKind::TryNotAllowed => "Question operator noe allowed here".into(),
            TypeErrorKind::TypeNotInferred => "Type can not be inferred".into(),
            TypeErrorKind::TypeAssertionFailed(msg) => {
                format!("Assertion failed during static analysis.\n\t{msg}")
            }
            TypeErrorKind::PatternAlreadyExhausted(pattern) => {
                let msg = "This pattern is already fully exhausted";
                if let MatchPattern::Assignee(a) = pattern {
                    format!("{msg} Note: '{a}' is treated as a variable name.")
                } else {
                    msg.into()
                }
            }
            TypeErrorKind::PatternNotExhausted(actual) => {
                format!("Type not fully exhausted by patterns. Found {actual}")
            }
            TypeErrorKind::MatchHasNoArms => "Match expression has no arms".into(),
        };

        Error::new(msg, source, self.loc.unwrap_or(Loc::start()))
    }
}

#[derive(Debug, Clone)]
pub struct ScriptError {
    pub kind: ScriptErrorKind,
    loc: Option<Loc>,
}

#[derive(Debug, Clone)]
pub enum ScriptErrorKind {
    NoValue,
    Error(ScriptValue),
    Panic(String),
    AssertionFailed(String),
}

impl ScriptError {
    pub fn new(kind: ScriptErrorKind) -> Self {
        Self { kind, loc: None }
    }

    pub fn no_value() -> Self {
        Self::new(ScriptErrorKind::NoValue)
    }

    pub fn error(value: ScriptValue) -> Self {
        Self::new(ScriptErrorKind::Error(value))
    }

    pub fn panic(error: impl ToString) -> Self {
        Self::new(ScriptErrorKind::Panic(error.to_string()))
    }

    pub fn expected_argument() -> Self {
        Self::panic("Expected argument")
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
            ScriptErrorKind::NoValue => "No value".into(),
            ScriptErrorKind::Error(err) => format!("Unhandled error: {err}"),
            ScriptErrorKind::Panic(error) => format!("Script panicked: {error}"),
            ScriptErrorKind::AssertionFailed(msg) => format!("Assertion failed: {msg}"),
        };

        Error::new(msg, source, self.loc.unwrap_or(Loc::start()))
    }
}

pub type TypeResult<T> = result::Result<T, TypeError>;
pub type ScriptResult<T> = result::Result<T, ScriptError>;

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
