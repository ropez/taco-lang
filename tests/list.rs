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


