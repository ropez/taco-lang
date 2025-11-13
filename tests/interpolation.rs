use taco::check_output;

#[test]
fn test_print_empty_string() {
    let src = r#"println("")"#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("\n", out);
}

#[test]
fn test_interpolation_escape_sequence() {
    let src = r#"print("$$foo$$bar$$")"#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("$foo$bar$", out);
}

#[test]
fn test_compact_interpolation() {
    let src = r#"
        prefix = "--"
        fruit = "banana"

        print("$prefix$fruit$prefix")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("--banana--", out);
}

#[test]
fn test_interpolation_expression() {
    let src = r#"
        prefix = "--"
        fruit = "banana"

        print("eq: ${fruit == prefix}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("eq: false", out);
}

#[test]
fn test_interpolation_call() {
    let src = r#"
        fun foo(arg: str): str {
            return "-- $arg"
        }

        fruit = "apple"
        print("call: '${foo(fruit)}'")

    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("call: '-- apple'", out);
}
