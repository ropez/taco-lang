use taco::check_output;

#[test]
fn test_function_calling_function() {
    let src = r#"
        fun indent(arg: str): str {
            return "- $arg"
        }

        fun bye(arg: str) {
            msg = "Good bye"
            println(msg, indent(arg))
        }

        bye("Have a nice day")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("Good bye\n- Have a nice day\n", out);
}

#[test]
fn test_positional_arguments() {
    let src = r#"
        fun args(first: int, second: int, third: int) {
            println("first: $first, second: $second, third: $third")
        }

        args(1, 2, 3)
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("first: 1, second: 2, third: 3\n", out);
}

#[test]
fn test_mixed_arguments() {
    let src = r#"
        fun args(first: str, second: str, third: str) {
            println("first: $first, second: $second, third: $third")
        }

        args("a", third: "c", second: "b")
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("first: a, second: b, third: c\n", out);
}
