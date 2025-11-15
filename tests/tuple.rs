use taco::check_output;

#[test]
fn test_simple_tuple() {
    let src = r#"
        tuple = ("apple", 50)

        (fruit, amount) = tuple

        print("$fruit, $amount")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("apple, 50", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_tuple_as_function_argument() {
    let src = r#"
        fun show(arg: (str, int)) {
            (fruit, amount) = arg
            print("$fruit, $amount")
        }

        show(("banana", 99))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana, 99", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_three_tuple_as_function_argument() {
    let src = r#"
        fun show(arg: (str, int, bool)) {
            (fruit, amount, ok) = arg
            print("$fruit, $amount, $ok")
        }

        show(("banana", 99, false))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana, 99, false", out),
        Err(err) => panic!("{err}"),
    };
}

