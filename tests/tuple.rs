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
fn test_named_tuple_as_function_argument() {
    let src = r#"
        fun show(arg: (a: int, b: int, c: int)) {
            print("$arg")
        }

        show(arg: (1, 2, 3))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("(a: 1, b: 2, c: 3)", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_call_with_named_tuple_args() {
    let src = r#"
        fun show(arg: (a: int, b: int, c: int)) {
            (a, b, c) = arg
            print("$a $b $c")
        }

        show((1, c: 3, b: 2))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("1 2 3", out),
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

#[test]
fn test_named_tuple() {
    let src = r#"
        pos = (lat: 23, lon: 69)
        println("$pos")
        (lat, lon) = pos
        println("$lat / $lon")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "(lat: 23, lon: 69)\n23 / 69\n"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_named_tuple_destruction() {
    let src = r#"
        fun foo(tup: (name: str, age: int)) {
            (name, age) = tup
            print("name: $name, age: $age")
        }

        # tup = (age: 25, name: "Erling")
        tup = ("Erling", 25)
        foo(tup, foo: tup)
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "name: Erling, age: 25"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_type_checking_formal_to_formal() {
    let src = r#"
        fun foo(tup: (str, name: str, age: int, more: str)): str {
            (_, name) = tup
            return name
        }

        fun bar(tup: (str, str, age: int, str)): str {
            foo(tup)
        }

        print(bar(("", "ok", 42, "")))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "ok"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_named_destruction_patterns() {
    let src = r#"
        (a, b, c, d) = (1, 2, 3, 4)
        println("$a$b$c$d")
        (a, b, c, d) = (1, 2, 3, d: 4)
        println("$a$b$c$d")
        (a, b, c, d) = (1, 2, d: 4, c: 3)
        println("$a$b$c$d")
        (a, b, c, d) = (1, d: 4, c: 3, b: 2)
        println("$a$b$c$d")
        (a, b, c, d) = (c: 3, d: 4, a: 1, b: 2)
        println("$a$b$c$d")
        (a, b, c, d) = (b: 2, c: 3, d: 4, 1)
        println("$a$b$c$d")
        (a, b, c, d) = (b: 2, 1, c: 3, d: 4)
        println("$a$b$c$d")
        (a, b, c, d) = (d: 4, 1, c: 3, 2)
        println("$a$b$c$d")
        (a, b, c, d) = (1, d: 4, 2, 3)
        println("$a$b$c$d")
        (a, b, c, d) = (1, c: 3, 2, 4)
        println("$a$b$c$d")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("1234\n".repeat(10), out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_nested_destruction() {
    let src = r#"
        (a, (b, (c, d))) = (1, (2, (3, 4)))
        println("$a$b$c$d")
        ((a, b), (c, d)) = ((1, 2), (3, 4))
        println("$a$b$c$d")
        ((a, (b, (c, d)))) = ((1, (2, (3, 4))))
        println("$a$b$c$d")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("1234\n".repeat(3), out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_nested_named_destruction() {
    let src = r#"
        (foo, bar) = (foo: (1, 2), bar: (3, 4))
        println("$foo $bar")
        (foo: (a, b), bar: (c, d)) = (foo: (1, 2), bar: (3, 4))
        println("$a $b $c $d")
    "#;

    // What about `(foo: a) = (foo: 1)` ?

    match check_output(src) {
        Ok(out) => assert_eq!("(1, 2) (3, 4)\n1 2 3 4\n", out),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_named_destruction_fails() {
    let src = r#"
        (a, b) = (a: 1, x: 2)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Element 'b' not found in (a: int, x: int)"),
    };
}

