use std::{
    error, fmt::{self, Write}, ops::Range, result
};

#[derive(Debug, Clone)]
pub struct Error {
    message: String,
    details: String,
}

impl Error {
    pub fn new(message: String, source: &str, loc: &Range<usize>) -> Self {
        let details = format_error_details(source, loc).unwrap_or_default();
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

fn format_error_details(source: &str, loc: &Range<usize>) -> result::Result<String, fmt::Error> {
    let mut buf = String::new();

    let mut pos = loc.start;
    let len = loc.end - loc.start;

    for (n, line) in source.lines().enumerate() {
        write!(buf, "\x1b[36m")?;
        write!(buf, "{:-5} | ", n + 1)?;
        write!(buf, "\x1b[0m")?;
        writeln!(buf, "{line}")?;
        if pos < line.len() {
            write!(buf, "      | ")?;
            write!(buf, "\x1b[33m")?;
            writeln!(buf, "{}{} here", " ".repeat(pos), "^".repeat(len))?;
            write!(buf, "\x1b[0m")?;
        }
        pos = pos.wrapping_sub(line.len() + 1);
    }

    Ok(buf)
}
