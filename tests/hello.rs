use taco::check_output;

#[test]
fn test_hello_world() {
    let src = r#"
        println("Hello, world")
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("Hello, world\n", out);
}
