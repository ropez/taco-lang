use taco::check_output;

#[test]
fn test_record_tuple_like() {
    let src = r#"
        record Point(x: int, y: int)

        p = Point(100, 200)

        x = p.x
        print("Point(${x}, ${p.y})")
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("Point(100, 200)", out);
}

#[test]
fn test_record_attribute_access() {
    let src = r#"
        record Point(x: int, y: int)

        p = Point(100, 200)

        if p.x == 100 {
            println("x is 100")
        }

        if 200 == p.y {
            println("y is 200")
        }

        if p.x == p.y {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("x is 100\ny is 200\nnot equal\n", out);
}

#[test]
fn test_record_struct_like() {
    let src = r#"
        record Person(
            name: str
            email: str
        )

        per = Person(
            email: "foo@bar.com"
            name: "per"
        )

        println(per.name)
        println(per.email)
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("per\nfoo@bar.com\n", out);
}

#[test]
fn test_record_with_method() {
    let src = r#"
        record Person(
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
