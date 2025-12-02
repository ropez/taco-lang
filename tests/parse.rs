use taco::check_output;

#[test]
fn test_parse_unnamed_record() {
    let src = r#"
        rec Point(int, int)

        p = Point::parse("100 200")

        print("$p")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Point(100, 200)", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_parse_named_record() {
    let src = r#"
        rec Point(x: int, y: int)

        p = Point::parse("100 200")

        print("$p")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Point(x: 100, y: 200)", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_parse_list() {
    let src = r#"
        rec Point(int, int)

        input = [
            "100  200"
            "400  600"
            "0 0"
        ]

        points = input.map(Point::parse)

        print("$points")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[Point(100, 200), Point(400, 600), Point(0, 0)]", out),
        Err(err) => panic!("{err}"),
    };
}
