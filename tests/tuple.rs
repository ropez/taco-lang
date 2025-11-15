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

#[test]
fn test_destructure_discard_value() {
    let src = r#"
        (_, fruit, _) = (99, "banana", false)
        print(fruit)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_discarded_value_not_assigned() {
    let src = r#"
        (_, fruit) = ("foo", "bar")
        print(_)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected identifier"),
    };
}
