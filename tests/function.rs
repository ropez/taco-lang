use taco::check_output;

#[test]
fn test_discarded_arg_not_assigned() {
    let src = r#"
        fun args(first: str, _: str) {
            print("${_}")
        }

        args("foo", "bar")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected identifier"),
    };
}

#[test]
fn test_return_not_allowed_outside_function() {
    let src = r#"
        return 10
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Unexpected return value: Expected no value, found 'int'"
        ),
    }
}

#[test]
fn test_return_not_allowed_in_contitional_outside_function() {
    let src = r#"
        if true {
            return 10
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Unexpected return value: Expected no value, found 'int'"
        ),
    }
}

#[test]
fn test_explicit_return_wrong_type() {
    let src = r#"
        fun square(n: int): bool {
            return n * n
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected 'bool', found 'int'"
        ),
    }
}

#[test]
fn test_implied_return_wrong_type() {
    let src = r#"
        fun square(n: int): bool {
            n * n
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected 'bool', found 'int'"
        ),
    }
}

#[test]
fn test_missing_return_statement() {
    let src = r#"
        fun foo(): str {
            println("foo")
            println("bar")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Missing return statement"),
    }
}

#[test]
fn test_missing_return_statement_with_conditional_at_end() {
    let src = r#"
        fun foo(cond: bool): str {
            println("foo")
            if cond {
                "foo"
            } else {
                "bar"
            }
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => {
            println!("{err}");
            assert_eq!(err.message, "Missing return statement")
        }
    }
}

#[test]
fn test_missing_argument_name_in_destruction() {
    let src = r#"
        fun foo(fruit: str, amount: int) {
            (fruit, count) = arguments
            print("$fruit $count")
        }

        foo("banana", 42)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            "Missing value for 'count': Found '(fruit: str, amount: int)'",
            err.message
        ),
    }
}
