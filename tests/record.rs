use taco::check_output;

#[test]
fn test_record_tuple_like() {
    let src = r#"
        record Point(x, y)

        p = Point(100, 200)

        print("Point(${p.x}, ${p.y})")
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
        record Point(x, y)

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
        record Person(name, email)

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
