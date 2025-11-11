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

