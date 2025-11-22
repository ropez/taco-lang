use std::fmt::{Display, Formatter, Result};

use crate::ident::Ident;

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

pub(crate) fn fmt_tuple<V, I>(f: &mut Formatter<'_>, mut items: I) -> Result
where
    V: Display,
    I: Iterator<Item = (Option<Ident>, V)>,
{
    write!(f, "(")?;

    if let Some(first) = items.next() {
        fmt_item(f, first)?;
        for val in items {
            write!(f, ", ")?;
            fmt_item(f, val)?;
        }
    }

    write!(f, ")")?;
    Ok(())
}

fn fmt_item<V>(f: &mut Formatter<'_>, o: (Option<Ident>, V)) -> Result
where
    V: Display,
{
    match o {
        (Some(name), t) => write!(f, "{name}: {t}"),
        (None, t) => write!(f, "{t}"),
    }
}
