use taco::check_output;

#[test]
fn test_simple_list() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        for fruit in fruits {
            println(fruit)
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("apple\norange\nbanana\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_list_as_function_argument() {
    let src = r#"
        fun show(items: [str]) {
            for it in items {
                println("'${it}'")
                return () # XXX Not implemented
            }
        }

        show(["foo", "bar"])
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("'foo'\n'bar'\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_print_lists() {
    let src = r#"
        foo = "foo"
        bar = "bar"

        println("${[]}")
        println("${[foo, bar]}")
        println("${[1, 2, 3, 4]}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[]\n[foo, bar]\n[1, 2, 3, 4]\n", out),
        Err(err) => panic!("{err}"),
    };
}


#[test]
fn test_empty_list_promotes() {
    let src = r#"
        a = []

        if true {
            a = a.push("foo") # Promotes [] to [str]
            for it in a {
                println(it)
            }
        }

        if true {
            a = a.push(10) # Promotes [] to [int]
            for it in a {
                println("$it")
            }
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("foo\n10\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promotes_in_call() {
    let src = r#"
        fun test(list: [bool]) {
            println("called with $list")
        }

        test([]) # Promotes [] to empty [bool]
        test(list: [])
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("called with []\ncalled with []\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promotes_list_items() {
    let src = r#"
        list = []
        list = list.push([10]) # list is now [[int]], list of int-lists
        println("1: $list")

        list = list.push([])
        println("2: $list")

        list = [[42], []]
        println("3: $list")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("1: [[10]]\n2: [[10], []]\n3: [[42], []]\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promote_in_return() {
    let src = r#"
        fun foo(): [int] {
            return []
        }

        fun bar(): [int] {
            []
        }

        fun poop(): [[int]] {
            [[]]
        }

        fun pooppoop(): [[[int]]] {
            [[[]]]
        }

        println("${foo()}")
        println("${bar()}")
        println("${poop()}")
        println("${pooppoop()}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[]\n[]\n[[]]\n[[[]]]\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_invalid_list_type_in_return() {
    let src = r#"
        fun foo(): [bool] {
            [[]]
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected [bool], found [[]]"),
    };
}
