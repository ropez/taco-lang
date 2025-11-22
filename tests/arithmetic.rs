use taco::check_output;

#[test]
fn test_simple_addition() {
    let src = r#"
        a = 10
        print("$a + 5 = ${a + 5}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("10 + 5 = 15", out);
}

#[test]
fn test_simple_subtraction() {
    let src = r#"
        a = 12
        print("$a - 5 = ${a - 5}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("12 - 5 = 7", out);
}

#[test]
fn test_simple_multiplication() {
    let src = r#"
        a = 7
        print("$a * 5 = ${a * 5}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("7 * 5 = 35", out);
}

#[test]
fn test_simple_division() {
    let src = r#"
        a = 15
        print("$a / 5 = ${a / 5}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("15 / 5 = 3", out);
}

#[test]
fn test_add_and_multiply_precedence() {
    let src = r#"
        a = 15
        print("2 + 3 * 4 = ${2 + 3 * 4}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("2 + 3 * 4 = 14", out);
}

#[test]
fn test_multiply_and_add_precedence() {
    let src = r#"
        a = 15
        print("2 * 3 + 4 = ${2 * 3 + 4}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("2 * 3 + 4 = 10", out);
}

#[test]
fn division_with_truncation() {
    let src = r#"
        print("${99 / 10}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("9", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn division_by_zero() {
    // TODO This behavior should be configurable, something like "--checked"

    let src = r#"
        a = 0
        print("${100 / a}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("NaN", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn adding_overflow() {
    // TODO This behavior should be configurable, something like "--checked"

    let src = r#"
        a = 9000000000000000000
        b = 9000000000000000000
        print("${a + b}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("NaN", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn subtracting_overflow() {
    // TODO This behavior should be configurable, something like "--checked"

    let src = r#"
        a = 9000000000000000000
        print("${0 - a - a}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("NaN", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn multiplication_overflow() {
    // TODO This behavior should be configurable, something like "--checked"

    let src = r#"
        a = 9000000000
        b = 9000000000
        print("${a * b}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("NaN", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn all_operations_with_nan() {
    // TODO This behavior should be configurable, something like "--checked"

    let src = r#"
        n = 1 / 0
        print("${1 + n}")
        print("${n + 1}")
        print("${1 - n}")
        print("${n - 1}")
        print("${1 * n}")
        print("${n * 1}")
        print("${1 / n}")
        print("${n / 1}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("NaN".repeat(8), out),
        Err(err) => panic!("{err}"),
    };
}
