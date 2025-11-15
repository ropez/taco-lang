use taco::check_output;

#[test]
fn test_simple_enum() {
    let src = r#"
        enum Hero(
            Fred
            Wilma
            Barney
        )

        hero = Hero::Fred

        print("$hero")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Fred", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_comparison() {
    let src = r#"
        enum Hero(
            Fred
            Wilma
            Barney
        )

        hero = Hero::Fred

        println("${hero == Hero::Fred}")
        println("${hero == Hero::Wilma}")
        println("${Hero::Wilma == Hero::Barney}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("true\nfalse\nfalse\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_comparison_not_allowed_for_different_enums() {
    let src = r#"
        enum Hero (Fred, Barney)
        enum Villain(Vandercave, Butterbean)

        if Hero::Fred != Villain::Butterbean {
            print("poop")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected Hero, found Villain"),
    };
}

