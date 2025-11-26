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
fn test_discard_arg_with_underscore() {
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
fn test_discard_unnamed_arg() {
    let src = r#"
        fun args(first: str, [str]) {
            print("$first")
        }

        args("foo", [""])
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
        Err(err) => assert_eq!(err.message, "Incompatible return type: Expected bool, found int")
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
fn test_implicit_return_inside_if_with_explicit_function_type() {
    let src = r#"
        enum Color(Green, Blue)

        fun select(col: Color): str {
            if col == Color::Green {
                "The color is green"
            } else {
                "Not green, probably blue"
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
fn test_implicit_return_inside_if_with_inferred_function_type() {
    let src = r#"
        enum Color(Green, Blue)

        fun select(col: Color) {
            if col == Color::Green {
                "The color is green"
            } else {
                "Not green, probably blue"
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
        Err(err) => assert_eq!(err.message, "Missing return statement")
    }
}

#[test]
fn test_missing_return_statement_in_one_condition() {
    let src = r#"
        fun foo(cond: bool): str {
            if cond {
                println("foo")
                return "foo"
            } else {
                println("bar")
                println("bar")
            }
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => {
            println!("{err}");
            assert_eq!(err.message, "Incompatible return type: Expected str, found str?")
        }
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
fn test_return_from_conditional_at_end() {
    let src = r#"
        fun foo(cond: bool): str {
            println("foo")
            if cond {
                return "foo"
            } else {
                return "bar"
            }
        }

        println(foo(false))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("foo\nbar\n", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_return_from_nested_conditional() {
    let src = r#"
        fun foo(a: bool, b: bool): str {
            if a {
                if b {
                    "ab"
                } else {
                    "a_"
                }
            } else {
                if b {
                    "_b"
                } else {
                    "__"
                }
            }
        }

        print(foo(false, true))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("_b", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_return_implicit_tuple() {
    let src = r#"
        fun foo(): (str, [int]) {
            ("banana", [])
        }

        (fruit, count) = foo()
        print(fruit)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_return_implicit_tuple_with_arg() {
    let src = r#"
        fun foo(arg: str): (str, [int]) {
            (arg, [])
        }

        (fruit, count) = foo("banana")
        print(fruit)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_arguments_access() {
    let src = r#"
        fun foo(fruit: str, count: int) {
            print("${arguments.fruit} ${arguments.count}")
        }

        foo("banana", 42)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana 42", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_manually_destruct_arguments() {
    let src = r#"
        fun foo(str, int, int) {
            (fruit, count, _) = arguments
            print("$fruit $count")
        }

        foo("banana", 42, 100)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana 42", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_manually_destruct_named_arguments() {
    let src = r#"
        fun foo(fruit: str, count: int) {
            (count, fruit) = arguments
            print("$fruit $count")
        }

        foo("banana", 42)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("banana 42", out),
        Err(err) => panic!("{err}"),
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
        Err(err) => assert_eq!("Element 'count' not found in (fruit: str, amount: int)", err.message),
    }
}

#[test]
fn test_apply_params() {
    let src = r#"
        fun foo(a: int, b: int, c: int, d: int) {
            println("$a$b$c$d")
        }

        # All must print "1234"
        foo(1, 2, 3, 4)
        foo(1, 2, 3, d: 4)
        foo(1, 2, d: 4, c: 3)
        foo(1, d: 4, c: 3, b: 2)
        foo(c: 3, d: 4, a: 1, b: 2)
        foo(b: 2, c: 3, d: 4, 1)
        foo(b: 2, 1, c: 3, d: 4)
        foo(d: 4, 1, c: 3, 2)
        foo(1, d: 4, 2, 3)
        foo(1, c: 3, 2, 4)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("1234\n".repeat(10), out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_apply_arguments_from_tuple() {
    let src = r#"
        fun foo(fruit: str, amount: int) {
            println("$amount $fruit")
        }

        # Normal call
        foo("banana", 42)

        # Using tuple
        tup = ("banana", 42)
        foo(=tup)
        foo(= ("banana", 42))

        # Using Rec
        rec Args(fruit: str, amount: int)
        args = Args(fruit: "banana", amount: 42)
        foo(=args)
        foo(= Args("banana", 42))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("42 banana\n".repeat(5), out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_forward_arguments() {
    let src = r#"
        fun foo(fruit: str, amount: int) {
            println("$amount $fruit")
        }

        fun bar(fruit: str, amount: int) {
            foo(=arguments)
        }

        bar("banana", 42)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("42 banana\n", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_forward_implied_arguments() {
    let src = r#"
        fun foo(fruit: str, amount: int) {
            println("$amount $fruit")
        }

        fun bar(fruit: str, amount: int) {
            foo(=)
        }

        bar("banana", 42)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("42 banana\n", out),
        Err(err) => panic!("{err}"),
    }
}
