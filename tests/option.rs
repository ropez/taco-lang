use taco::check_output;

#[test]
fn test_expected_str_found_opt() {
    let src = r#"
        fun foo(a: str) {}

        opt = [""].find("")
        foo(opt)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected str, found str?"),
    };
}

#[test]
fn test_expected_int_found_opt() {
    let src = r#"
        fun foo(a: int) {}

        opt = [0].find(0)
        foo(opt)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected int, found int?"),
    };
}

#[test]
fn test_unexpected_opt_str_in_interp() {
    let src = r#"
        opt = [""].find("")
        print("${opt}")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected str got str?"),
    };
}

#[test]
fn test_unexpected_opt_int_in_interp() {
    let src = r#"
        opt = [0].find(0)
        print("${opt}")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected int got int?"),
    };
}

#[test]
fn test_opt_return_type_annotation() {
    let src = r#"
        fun find_foo(list: [str]): str? {
            return list.find("foo")
        }

        if t in find_foo(["foo"]) {
            print("ok")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("ok", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_opt_arg_type_annotation() {
    let src = r#"
        fun foo(a: str?) {
            if t in a {
                print("ok")
            }
        }

        foo(["foo"].find("foo"))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("ok", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_conditional_return() {
    let src = r#"
        fun foo(condition: bool): str? {
            if condition {
                return "ok"
            }
        }

        if s in foo(true) {
            println(s)
        }

        if s in foo(false) {
            println(s)
        } else {
            println("nothing")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("ok\nnothing\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_explicit_return_nothing() {
    let src = r#"
        fun foo(condition: bool): str? {
            if !condition { return }

            return "ok"
        }

        if s in foo(true) {
            println(s)
        }

        if s in foo(false) {
            println(s)
        } else {
            println("nothing")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(vec!["ok", "nothing"], out.lines().collect::<Vec<_>>()),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_tuple_destructuring() {
    let src = r#"
        fun foo(condition: bool): (str, int)? {
            if condition {
                return ("ok", 42)
            }
        }

        if (s, n) in foo(true) {
            print("$s and $n")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("ok and 42", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_nested_opt() {
    // The language doesn't allow something like str??, it simply collapses to str?

    let src = r#"
        fun foo(a: bool): str? {
            if a {
                "foobar"
            }
        }

        fun bar(a: bool, b: bool): str? {
            if a {
                foo(b)
            }
        }

        if x in bar(true, true) {
            print(x)
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("foobar", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_nested_opt_empty() {
    let src = r#"
        fun foo(a: bool): str? {
            if a {
                "foobar"
            }
        }

        fun bar(a: bool, b: bool): str? {
            if a {
                foo(b)
            }
        }

        if x in bar(true, false) {
            print(x)
        } else {
            print("nothing")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("nothing", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_nested_opt_trick_with_tuple() {
    // If someone really wants to have an opt that contains an opt, use the "tuple trick"

    let src = r#"
        fun foo(a: bool): str? {
            if a {
                "foobar"
            }
        }

        fun bar(a: bool, b: bool): (str?)? {
            if a {
                (foo(b))
            }
        }

        if (x) in bar(true, false) {
            if y in x {
                print(y)
            } else {
                print("nothing inside")
            }
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("nothing inside", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_opt_list_of_opt() {
    let src = r#"
        fun any(opts: [str?]?): bool {
            if opts in opts {
                for it in opts {
                    if _ in it {
                        return true
                    }
                }
            }

            return false
        }

        items = ["foo", "bar"]

        if any([items.find("a"), items.find("foo")]) {
            print("found")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("found", out),
        Err(err) => panic!("{err}"),
    };
}

#[ignore]
#[test]
fn test_question_operator() {
    let src = r#"
        available_fruits = [
            "apple"
            "orange"
            "banana"
        ]

        fun try_buy(fruit: str, amount: int): str? {
            f = available_fruits.find(fruit)?
            return "$amount ${f}s"
        }

        if r in try_buy("banana", 42) {
            println(r)
        } else {
            println("no banana")
        }

        if r in try_buy("pineapple", 36) {
            println(r)
        } else {
            println("no pineapples")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("42 bananas\nno pineapples\n", out),
        Err(err) => panic!("{err}"),
    };
}
