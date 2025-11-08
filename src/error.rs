use std::{
    error,
    fmt::{self, Write},
    result,
};

#[derive(Debug, Clone)]
pub struct Error {
    message: String,
    details: String,
}

impl Error {
    pub fn new(message: String, source: &str, section: &str) -> Self {
        let details = format_error_details(source, section).unwrap_or_default();
        Self { message, details }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.message)?;
        writeln!(f)?;
        writeln!(f, "{}", self.details)?;
        Ok(())
    }
}

impl error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

fn format_error_details(source: &str, section: &str) -> result::Result<String, fmt::Error> {
    let mut buf = String::new();

    for (n, line) in source.lines().enumerate() {
        write!(buf, "\x1b[36m")?;
        write!(buf, "{:-5} | ", n + 1)?;
        write!(buf, "\x1b[0m")?;
        writeln!(buf, "{line}")?;
        if let Some(offset) = substring_offset(line, section) {
            write!(buf, "      | ")?;
            write!(buf, "\x1b[33m")?;
            writeln!(buf, "{}{} here", " ".repeat(offset), "^".repeat(section.len()))?;
            write!(buf, "\x1b[0m")?;
        }
    }

    Ok(buf)
}

fn substring_offset(outer: &str, inner: &str) -> Option<usize> {
    let self_beg = outer.as_ptr() as usize;
    let inner = inner.as_ptr() as usize;
    if inner < self_beg || inner > self_beg.wrapping_add(outer.len()) {
        None
    } else {
        Some(inner.wrapping_sub(self_beg))
    }
}
