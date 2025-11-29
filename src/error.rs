use std::{
    error,
    fmt::{self, Write},
    result,
};

use crate::{ident::Ident, lexer::Loc, validate::{ArgumentType, ScriptType}};

pub enum ArgumentError {
    MissingArgument(Option<Ident>),
    WrongArgumentType(ScriptType, ArgumentType),
    UnexpectedArgument(ArgumentType),
}

// Promote ArgumentError to "Error" below

impl ArgumentError {
    pub(crate) fn into_source_error(self, source: &str, loc: Loc) -> Error {
        match self {
            ArgumentError::MissingArgument(ident) => {
                if let Some(ident) = ident {
                    Error::new(format!("Missing argument: {ident}"), source, Loc::start())
                } else {
                    Error::new(format!("Missing argument"), source, loc)
                }
            }
            ArgumentError::WrongArgumentType(expected, argument) => Error::new(
                format!("Expected {expected}, found {}", argument.value.as_ref()),
                source,
                argument.value.loc,
            ),
            ArgumentError::UnexpectedArgument(arg) => Error::new(
                "Unexpected argument".into(),
                source,
                arg.value.loc
            ),
        }
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
