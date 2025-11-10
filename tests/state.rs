use taco::check_output;

#[test]
fn test_get_and_set_primitive() {
    let src = r#"
        a = state(10)

        if a.get() == 10 {
            a.set(20)
        }

        println("${a.get()}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("20\n", out);
}

