use taco::check_output;

#[test]
fn test_invalid_list_type_in_return() {
    let src = r#"
        fun foo(): [bool] {
            [[]]
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected '[bool]', found '[[]]'"
        ),
    };
}
