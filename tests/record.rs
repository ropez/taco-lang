use taco::check_output;

#[test]
fn test_record_tuple_like() {
    let src = r#"
        rec Point(x: int, y: int)

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
        rec Point(x: int, y: int)

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
        rec Person(
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
fn test_record_with_unnamed_params() {
    let src = r#"
        rec Person(str, str)

        per = Person("foo@bar.com", "per")

        println("$per")

        (email, name) = per
        println("$name")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "Person(foo@bar.com, per)\nper\n"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_apply_arguments_from_tuple() {
    let src = r#"
        rec Person(name: str, age: int)

        args = ("per", 42)
        per = Person(=args)

        print("$per")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "Person(name: per, age: 42)"),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_pass_record_as_function() {
    let src = r#"
        rec Person(name: str, age: int)

        args = [
            ("per", 42)
            ("kari", 36)
        ]

        people = args.map_to(Person)

        print("$people")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(
            out,
            "[Person(name: per, age: 42), Person(name: kari, age: 36)]"
        ),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_apply_params() {
    let src = r#"
        rec Foo(a: int, b: int, c: int, d: int)

        fun dump(foo: Foo) {
            println("$foo")
        }

        dump(Foo(1, 2, 3, 4))
        dump(Foo(1, 2, 3, d: 4))
        dump(Foo(1, 2, d: 4, c: 3))
        dump(Foo(1, d: 4, c: 3, b: 2))
        dump(Foo(c: 3, d: 4, a: 1, b: 2))
        dump(Foo(b: 2, c: 3, d: 4, 1))
        dump(Foo(b: 2, 1, c: 3, d: 4))
        dump(Foo(d: 4, 1, c: 3, 2))
        dump(Foo(1, d: 4, 2, 3))
        dump(Foo(1, c: 3, 2, 4))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Foo(a: 1, b: 2, c: 3, d: 4)\n".repeat(10), out),
        Err(err) => panic!("{err}"),
    }
}
