use std::{env::args, fs, io::stdout};

use taco::{check_call, run_tests};

// TODO Command line
// Try to use subcommands, but default to "run" if no subcommand given.
//
// Possible subcommands:
// - run
// - check
// - format
// - lint?
// - interactive/repl?

fn main() {
    let mut args = args().peekable();
    let _ = args.next().expect("script name");

    let testing = args.next_if_eq("test").is_some();

    let script = args.next().expect("one argument");

    let src = fs::read_to_string(script).unwrap();

    let result = if testing {
        run_tests(&src)
    } else {
        check_call(&src, stdout())
    };

    if let Err(err) = result {
        eprintln!("{err}");
    }
}
