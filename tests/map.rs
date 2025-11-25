use taco::check_output;

#[test]
fn test_simple_map() {
    let src = r#"
        fun double(a: int) { a + a }

        result = [1, 2, 3].map(double)

        print("$result")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[2, 4, 6]", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_map_to_string() {
    let src = r#"
        fun fmt(p: (a: int, b: int)) { "'${p.a}x${p.b}'" }

        result = [(5, 2), (3, 1), (4, 6)].map(fmt)

        for i in result {
            println(i)
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("'5x2'\n'3x1'\n'4x6'\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn function_must_take_correct_arguments() {
    let src = r#"
        fun fmt(a: str) { "$a" }

        result = [1, 2, 3].map(fmt)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(out) => assert_eq!(out.message, "Expected function to take int"),
    };
}
