use std::{env::args, fs, io::stdout};

use taco::check_call;

fn main() {
    let mut args = args();
    let _ = args.next().expect("script name");
    let script = args.next().expect("one argument");

    let src = fs::read_to_string(script).unwrap();

    if let Err(err) = check_call(&src, stdout()) {
        eprintln!("{err}");
    }
}
