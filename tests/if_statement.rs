
use taco::check_output;

#[test]
fn test_arg_equals_string() {
    let src = r#"
        arg = "taco"
        if arg == "taco" {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("equal\n", out);
}

#[test]
fn test_string_equals_arg() {
    let src = r#"
        arg = "taco"
        if "taco" == arg {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("equal\n", out);
}

#[test]
fn test_arg_not_equals_string() {
    let src = r#"
        arg = "poop"
        if "taco" == arg {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("not equal\n", out);
}

#[test]
fn test_function_returns_true() {
    let src = r#"
        fun test(arg) {
            return arg == "taco"
        }
        if test("taco") {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("equal\n", out);
}

#[test]
fn test_function_returns_false() {
    let src = r#"
        fun test(arg) {
            return arg == "taco"
        }
        if test("poop") {
            println("equal")
        } else {
            println("not equal")
        }
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("not equal\n", out);
}
