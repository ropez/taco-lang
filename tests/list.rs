use taco::check_output;

#[test]
fn test_simple_list() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        for fruit in fruits {
            println(fruit)
        }
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("apple\norange\nbanana\n", out);
}


#[test]
fn test_list_push() {
    let src = r#"
        banana = "banana"
        fruits = [
            "apple"
            banana
        ].push("orange")

        fruits = fruits.push("kiwi")

        for fruit in fruits.push("pineapple") {
            println(fruit)
        }
    "#;

    let out = match check_output(src) {
        Ok(out) => out,
        Err(err) => panic!("{err}"),
    };

    assert_eq!("apple\nbanana\norange\nkiwi\npineapple\n", out);
}

