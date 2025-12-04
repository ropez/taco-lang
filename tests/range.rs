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

#[test]
fn test_range_as_arg() {
    let src = r#"
        fun foo(rng: range) {
            for i in rng {
                println("$i")
            }
        }

        foo(3..5)
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("3\n4\n5\n", out);
}
