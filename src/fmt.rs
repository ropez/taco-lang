use std::fmt::{Display, Formatter, Result};


pub(crate) fn fmt_tuple(f: &mut Formatter<'_>, values: &[impl Display]) -> Result {
    write!(f, "(")?;
    fmt_inner_list(f, values)?;
    write!(f, ")")?;
    Ok(())
}

pub(crate) fn fmt_inner_list(f: &mut Formatter<'_>, values: &[impl Display]) -> Result {
    let mut iter = values.iter();
    if let Some(first) = iter.next() {
        write!(f, "{first}")?;
        for val in iter {
            write!(f, ", {val}")?;
        }
    }
    Ok(())
}

