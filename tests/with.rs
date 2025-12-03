use taco::check_output;

// #[test]
fn test_tuple_with_method() {
    let src = r#"
        old = (
            email: "foo@bar.com"
            name: "per"
        )

        new = old.with(email: "new@email.com")

        println(old.name)
        println(old.email)
        println(new.name)
        println(new.email)
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("per\nfoo@bar.com\nper\nnew@email.com\n", out);
}

#[test]
fn test_record_with_method() {
    let src = r#"
        rec Person(
            name: str
            email: str
        )

        old = Person(
            email: "foo@bar.com"
            name: "per"
        )

        new = old.with(email: "new@email.com")

        println(old.name)
        println(old.email)
        println(new.name)
        println(new.email)
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("per\nfoo@bar.com\nper\nnew@email.com\n", out);
}
