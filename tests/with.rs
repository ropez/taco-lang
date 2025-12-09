use taco::check_output;

#[test]
fn test_record_with_method_invalid_type() {
    let src = r#"
        rec Person(name: str, email: str)
        per = Person(name: "per", email: "")

        per = per.with(email: false)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'str?', found 'bool'"),
    };
}
