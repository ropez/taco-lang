use taco::check_output;

#[test]
fn test_simple_type() {
    let src = r#"
        s = "Hello"
        print("${typeof(s)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("str", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_complex_type() {
    let src = r#"
        s = ("foo", bar: (a: 10, b: [1, 2]), [])
        print("${typeof(s)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("(str, bar: (a: int, b: [int]), [])", out),
        Err(err) => panic!("{err}"),
    }
}

// #[test]
fn test_function_types() {
    let src = r#"
        fun foo(a: int, b: bool): str? { "foo" }
        print("${typeof(foo)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("(str, bar: (a: int, b: [int]), [])", out),
        Err(err) => panic!("{err}"),
    }
}
