use taco::check_output;

#[test]
fn test_simple_iteration() {
    let src = r#"
        list = [1, 2, 3]
        for item in list {
            println("$item")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "1\n2\n3\n"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_not_allowed() {
    let src = r#"
        list = []
        for item in list {
            print("")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "List is always empty"),
    };
}

#[test]
fn test_empty_list_literal_not_allowed() {
    let src = r#"
        for item in [] {
            print("")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "List is always empty"),
    };
}

#[test]
fn test_empty_list_is_allowed() {
    let src = r#"
        fun foo(): [int] {
            println("foo")
            return []
        }

        for item in foo() {
            println("not reached")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "foo\n"),
        Err(err) => panic!("{err}"),
    };
}



