use std::{
    io,
    sync::{Arc, Mutex},
};

use crate::Builder;

pub(crate) mod list;
pub(crate) mod math;
pub(crate) mod opt;
pub(crate) mod panic;
pub(crate) mod parse;
pub(crate) mod pipe;
pub(crate) mod print;
pub(crate) mod state;
pub(crate) mod string;
pub(crate) mod type_of;
pub(crate) mod with;

#[cfg(feature = "fs")]
pub(crate) mod fs;

#[cfg(feature = "http")]
pub(crate) mod http;

#[cfg(feature = "process")]
pub(crate) mod process;

#[cfg(feature = "json")]
pub(crate) mod json;

pub fn build<O>(builder: &mut Builder, out: Arc<Mutex<O>>)
where
    O: io::Write + Send + Sync + 'static,
{
    string::build(builder);
    math::build(builder);
    list::build(builder);
    opt::build(builder);
    pipe::build(builder);
    panic::build(builder);
    with::build(builder);
    state::build(builder);
    print::build(builder, Arc::clone(&out));
    parse::build(builder);
    type_of::build(builder);

    #[cfg(feature = "fs")]
    fs::build(builder);

    #[cfg(feature = "http")]
    http::build(builder);

    #[cfg(feature = "process")]
    process::build(builder, Arc::clone(&out));

    #[cfg(feature = "json")]
    json::build(builder);
}
