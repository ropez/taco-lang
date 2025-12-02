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

    match check_output(src) {
        Ok(out) => assert_eq!("apple\norange\nbanana\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_list_as_function_argument() {
    let src = r#"
        fun show(items: [str]) {
            for it in items {
                println("'${it}'")
            }
        }

        show(["foo", "bar"])
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("'foo'\n'bar'\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_print_lists() {
    let src = r#"
        foo = "foo"
        bar = "bar"

        println("${[]}")
        println("${[foo, bar]}")
        println("${[1, 2, 3, 4]}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[]\n[foo, bar]\n[1, 2, 3, 4]\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promotes() {
    let src = r#"
        a = []

        println(typeof(a))

        if true {
            a = a.push("foo")
            println(typeof(a))
        }

        if true {
            a = a.push(10) # Promotes [] to [int]
            println(typeof(a))
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[]\n[str]\n[int]\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promotes_in_call() {
    let src = r#"
        fun test(list: [bool]) {
            println("called with $list")
        }

        test([]) # Promotes [] to empty [bool]
        test(list: [])
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("called with []\ncalled with []\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promotes_list_items() {
    let src = r#"
        list = []
        list = list.push([10]) # list is now [[int]], list of int-lists
        println("1: $list")

        list = list.push([])
        println("2: $list")

        list = [[42], []]
        println("3: $list")

        list = [[], [42]]
        println("4: $list")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(
            "1: [[10]]\n2: [[10], []]\n3: [[42], []]\n4: [[], [42]]\n",
            out
        ),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_empty_list_promote_in_return() {
    let src = r#"
        fun foo(): [int] {
            return []
        }

        fun bar(): [int] {
            []
        }

        fun poop(): [[int]] {
            [[]]
        }

        fun pooppoop(): [[[int]]] {
            [[[]]]
        }

        println("${foo()}")
        println("${bar()}")
        println("${poop()}")
        println("${pooppoop()}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[]\n[]\n[[]]\n[[[]]]\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_invalid_list_type_in_return() {
    let src = r#"
        fun foo(): [bool] {
            [[]]
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected '[bool]', found '[[]]'"
        ),
    };
}

#[test]
fn test_simple_map() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        fun quote(name: str) {
            "'$name'"
        }

        for fruit in fruits.map(quote) {
            println(fruit)
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("'apple'\n'orange'\n'banana'\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_map_as_tuple() {
    let src = r#"
        fruits = [
            ("apple", 5)
            ("orange", 15)
            ("banana", 42)
        ]

        fun quote(fruit: str, amount: int) {
            "$amount ${fruit}s"
        }

        for fruit in fruits.map_to(quote) {
            println(fruit)
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("5 apples\n15 oranges\n42 bananas\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_zip_combines_two_lists() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        heroes = [ "fred", "barney" ]

        zipped = [].zip(fruits, heroes, [1, 2, 3])
        print("$zipped")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("[(apple, fred, 1), (orange, barney, 2)]", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_unzip_splits_list_of_tuples() {
    let src = r#"
        items = [
            ("apple", 5)
            ("orange", 15)
            ("banana", 42)
        ]

        unzipped = items.unzip()
        print("$unzipped")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("([apple, orange, banana], [5, 15, 42])", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_find_an_item() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        if item in fruits.find("banana") {
            print("found $item")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("found banana", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_find_no_item() {
    let src = r#"
        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        if item in fruits.find("pineapple") {
            print("found $item")
        } else {
            print("no such item")
        }
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("no such item", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_sum_of_numbers() {
    let src = r#"
        numbers = [16, 27, 3, 32, 11, 17, 34]

        print("${numbers.sum() * 1}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("140", out),
        Err(err) => panic!("{err}"),
    };
}
#[test]
fn test_sort_numbers() {
    let src = r#"
        numbers = [16, 27, 3, 32, 11, 17, 34]
        sorted = numbers.sort().sort()

        println("$numbers")
        println("$sorted")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(
            "[16, 27, 3, 32, 11, 17, 34]\n[3, 11, 16, 17, 27, 32, 34]\n",
            out
        ),
        Err(err) => panic!("{err}"),
    };
}
