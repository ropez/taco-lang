use taco::check_output;

#[test]
fn test_range() {
    let src = r#"
        for i in 3..5 {
            println("$i")
        }
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("3\n4\n5\n", out);
}
