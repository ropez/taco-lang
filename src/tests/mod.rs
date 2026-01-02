mod interpolation;
mod validation;

use crate::check_output;

#[test]
fn test_hello_world() {
    let src = r#"
        println("Hello, world")
    "#;

    let out = check_output(src).unwrap();

    assert_eq!("Hello, world\n", out);
}

#[test]
fn test_question_operator() {
    let src = r#"
        available_fruits = [
            "apple"
            "orange"
            "banana"
        ]

        fun try_buy(fruit: str, amount: int): str? {
            f = available_fruits.find(fruit)?
            return "$amount ${f}s"
        }

        if r in try_buy("banana", 42) {
            println(r)
        } else {
            println("no banana")
        }

        if r in try_buy("pineapple", 36) {
            println(r)
        } else {
            println("no pineapples")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("42 bananas\nno pineapples\n", out),
        Err(err) => panic!("{err}"),
    };
}
