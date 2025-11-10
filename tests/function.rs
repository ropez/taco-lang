use taco::check_output;

#[test]
fn test_function_calling_function() {
    let src = r#"
        fun indent(arg) {
            return "- $arg"
        }

        fun bye(arg) {
            msg = "Good bye"
            println(msg, indent(arg))
        }

        bye("Have a nice day")
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("Good bye\n- Have a nice day\n", out);
}
