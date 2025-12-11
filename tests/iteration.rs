use taco::check_output;

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
