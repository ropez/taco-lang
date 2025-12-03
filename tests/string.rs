use taco::check_output;

#[test]
fn test_lines() {
    let src = r#"
        fruits = "
apple
orange
banana"

        print("${fruits.lines()}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[apple, orange, banana]", out),
        Err(err) => panic!("{err}"),
    };
}
