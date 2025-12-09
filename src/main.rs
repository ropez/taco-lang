use std::{
    env::args,
    fs,
    io::{self, stdout},
    path::Path,
};

use taco::{TestStats, check_call, run_tests};

// TODO Command line
// Try to use subcommands, but default to "run" if no subcommand given.
//
// Possible subcommands:
// - run
// - check
// - format
// - lint?
// - interactive/repl?

fn main() -> io::Result<()> {
    let mut args = args().peekable();
    let _ = args.next().expect("script name");

    let testing = args.next_if_eq("test").is_some();

    let result = if testing {
        if let Some(path) = args.next() {
            run_test_file(&path);

            Ok(())
        } else {
            let mut stats = TestStats::default();
            for entry in fs::read_dir("tests")? {
                let path = entry?.path();
                let s = path.as_os_str().to_str().expect("path to str");
                if path.is_file() && s.ends_with(".tc") {
                    eprintln!("\nRunning {path:?}");
                    stats.update(&run_test_file(path));
                }
            }

            eprintln!();
            if stats.failed == 0 && stats.errors == 0 {
                eprintln!("All tests passed!");
            } else {
                eprintln!(
                    "Test results: {} succeeded, {} failed, {} errors",
                    stats.succeeded, stats.failed, stats.errors
                );
            }
            Ok(())
        }
    } else {
        let script = args.next().expect("one argument");
        let src = fs::read_to_string(script).unwrap();

        check_call(&src, stdout())
    };

    if let Err(err) = result {
        eprintln!("{err}");
    }

    Ok(())
}

fn run_test_file<P>(path: P) -> TestStats
where
    P: AsRef<Path>,
{
    let src = fs::read_to_string(path).unwrap();
    match run_tests(&src) {
        Ok(stats) => stats,
        Err(err) => {
            eprintln!("{err}");
            TestStats::error()
        }
    }
}
