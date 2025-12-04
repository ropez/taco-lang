use taco::check_output;

#[test]
fn test_lines() {
    let src = r#"
        fruits = "
apple
orange
banana"

        print("${fruits.lines()}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[apple, orange, banana]", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_split() {
    let src = r#"
        s = "1,123,foobar"
        tokens = s.split(",")
        print("$tokens")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[1, 123, foobar]", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_split_at() {
    let src = r#"
        s = "superstar"
        print("${s.split_at(5)}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("(super, star)", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_split_into_named_tuple() {
    let src = r#"
        s = "superstar"
        (first, second) = s.split_at(5)
        print("A $first cool $second")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("A super cool star", out),
        Err(err) => panic!("{err}"),
    };
}
