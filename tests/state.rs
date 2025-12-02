use taco::check_output;

#[test]
fn test_normal_variables_are_immutable() {
    let src = r#"
        a = 10

        if a == 10 {
            a = a + 5
        }

        fun foo() {
            a = 20
        }

        foo()
        print("$a")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("10", out);
}

#[test]
fn test_get_and_set_primitive() {
    let src = r#"
        a = state(10)

        if a.get() == 10 {
            a.set(a.get() + 5)
        }

        if 15 == a.get() {
            a.set(5 + a.get())
        }

        print("${a.get()}")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("20", out);
}
