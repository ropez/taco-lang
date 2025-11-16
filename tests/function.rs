use taco::check_output;

#[test]
fn test_function_calling_function() {
    let src = r#"
        fun indent(arg: str): str {
            return "- $arg"
        }

        fun bye(arg: str) {
            msg = "Good bye"
            println(msg)
            println(indent(arg))
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

#[test]
fn test_can_discard_args() {
    let src = r#"
        fun args(first: str, _: str) {
            print("$first")
        }

        args("foo", "bar")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("foo", out),
        Err(err) => panic!("{err}"),
    };

}

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
        Err(err) => assert_eq!(err.message, "Unexpected return value")
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
        Err(err) => assert_eq!(err.message, "Unexpected return value")
    }
}

#[test]
fn test_implied_return() {
    let src = r#"
        fun square(n: int): int {
            n * n
        }
        n = square(2)
        print("${square(n)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("16", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_implied_return_with_implied_type() {
    let src = r#"
        fun square(n: int) { n * n }
        n = square(2)
        print("${square(n)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("16", out),
        Err(err) => panic!("{err}"),
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
        Err(err) => assert_eq!(err.message, "Expected bool, found int")
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
        Err(err) => assert_eq!(err.message, "Expected bool, found int")
    }
}

#[test]
fn test_explicit_return_inside_if() {
    let src = r#"
        enum Color(Green, Blue)

        fun select(col: Color): str {
            if col == Color::Green {
                return "The color is green"
            } else {
                return "Not green, probably blue"
            }
        }

        println(select(Color::Green))
        println(select(Color::Blue))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("The color is green\nNot green, probably blue\n", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_explicit_return_inside_list_iteration() {
    let src = r#"
        enum Color(Green, Blue)

        fun search(colors: [Color]): str {
            for col in colors {
                if col == Color::Green {
                    return "Found green"
                }
            }
            return "Not found"
        }

        println(search([Color::Blue, Color::Green]))
        println(search([Color::Blue]))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Found green\nNot found\n", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_explicit_return_inside_range_iteration() {
    let src = r#"
        enum Color(Green, Blue)

        fun search(r: int): str {
            for i in 0..100 {
                if i == r {
                    return "Found $r"
                }
            }
            return "Not found"
        }

        println(search(42))
        println(search(999))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Found 42\nNot found\n", out),
        Err(err) => panic!("{err}"),
    }
}
