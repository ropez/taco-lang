use std::{
    env::args,
    fs,
    io::{self, stdout},
    path::Path,
};

use taco::{TestError, TestStats, check_call, run_tests};

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
        testing_main(args.next())?;

        Ok(())
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

fn testing_main(arg: Option<String>) -> Result<(), io::Error> {
    let stats = if let Some(path) = arg {
        run_test_file(&path)
    } else {
        let mut stats = TestStats::default();
        for entry in fs::read_dir("tests")? {
            let path = entry?.path();
            let s = path.as_os_str().to_str().expect("path to str");
            if path.is_file() && s.ends_with(".tc") {
                eprintln!("\nRunning {path:?}");
                stats.update(run_test_file(path));
            }
        }

        stats
    };

    for err in &stats.errors {
        eprintln!();
        eprintln!("=== ERROR in {:?} ===", err.file_name);
        eprintln!("{}", err.error);
    }

    for err in &stats.failures {
        eprintln!();
        eprintln!("=== Failure: {} ===", err.test_name);
        eprintln!("{}", err.error);

        if !err.output.is_empty() {
            eprintln!();
            eprintln!("=== Captured output ===");
            eprintln!("{}", err.output);
            eprintln!();
        }
    }
    eprintln!();

    if stats.failures.is_empty() && stats.errors.is_empty() {
        eprintln!("All {} tests passed!", stats.succeeded);
    } else {
        eprintln!(
            "Test results: {} succeeded, {} failed, {} errors",
            stats.succeeded,
            stats.failures.len(),
            stats.errors.len()
        );
    }

    Ok(())
}

fn run_test_file<P>(path: P) -> TestStats
where
    P: AsRef<Path>,
{
    let src = fs::read_to_string(&path).unwrap();
    match run_tests(&src) {
        Ok(stats) => stats,
        Err(err) => {
            eprintln!("{err}");
            TestStats::error(TestError {
                file_name: path.as_ref().into(),
                error: err.to_string(),
            })
        }
    }
}
