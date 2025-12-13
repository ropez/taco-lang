use crate::check_output;

#[test]
fn test_discarded_arg_not_assigned() {
    let src = r#"
        fun args(first: str, _: str) {
            print("${_}")
        }

        args("foo", "bar")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected identifier"),
    };
}

#[test]
fn test_return_not_allowed_outside_function() {
    let src = r#"
        return 10
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Unexpected return value: Expected no value, found 'int'"
        ),
    }
}

#[test]
fn test_return_not_allowed_in_contitional_outside_function() {
    let src = r#"
        if true {
            return 10
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Unexpected return value: Expected no value, found 'int'"
        ),
    }
}

#[test]
fn test_explicit_return_wrong_type() {
    let src = r#"
        fun square(n: int): bool {
            return n * n
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected 'bool', found 'int'"
        ),
    }
}

#[test]
fn test_implied_return_wrong_type() {
    let src = r#"
        fun square(n: int): bool {
            n * n
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            err.message,
            "Incompatible return type: Expected 'bool', found 'int'"
        ),
    }
}

#[test]
fn test_missing_return_statement() {
    let src = r#"
        fun foo(): str {
            println("foo")
            println("bar")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Missing return statement"),
    }
}

#[test]
fn test_missing_return_statement_with_conditional_at_end() {
    let src = r#"
        fun foo(cond: bool): str {
            println("foo")
            if cond {
                "foo"
            } else {
                "bar"
            }
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => {
            println!("{err}");
            assert_eq!(err.message, "Missing return statement")
        }
    }
}

#[test]
fn test_missing_argument_name_in_destruction() {
    let src = r#"
        fun foo(fruit: str, amount: int) {
            (fruit, count) = arguments
            print("$fruit $count")
        }

        foo("banana", 42)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(
            "Missing value for 'count': Found '(fruit: str, amount: int)'",
            err.message
        ),
    }
}

#[test]
fn test_comparison_not_allowed_for_different_enums() {
    let src = r#"
        enum Hero { Fred, Barney }
        enum Villain { Vandercave, Butterbean }

        if Hero::Fred != Villain::Butterbean {
            print("poop")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => {
            println!("{err}");
            assert_eq!(err.message, "Expected 'Hero', found 'Villain'")
        }
    };
}

#[test]
fn fails_for_missing_value() {
    let src = r#"
        enum City {
            Paris
            London
            Madrid
        }

        enum VacationPlan {
            StayAtHome
            TravelTo(City)
        }

        fun foo(plan: VacationPlan) {
            print("$plan")
        }

        foo(VacationPlan::TravelTo)
    "#;

    match check_output(src) {
        Ok(out) => panic!("Expected error, got {out}"),
        Err(err) => assert_eq!(
            err.message,
            "Expected 'VacationPlan', found 'fun(City): VacationPlan'"
        ),
    };
}

#[test]
fn fails_for_unexpected_call() {
    let src = r#"
        enum City {
            Paris
            London
            Madrid
        }

        enum VacationPlan {
            StayAtHome
            TravelTo(City)
        }

        fun foo(plan: VacationPlan) {
            print("$plan")
        }

        foo(VacationPlan::StayAtHome())
    "#;

    match check_output(src) {
        Ok(out) => panic!("Expected error, got {out}"),
        Err(err) => assert_eq!(err.message, "Expected a callable, found 'VacationPlan'"),
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
fn test_record_with_method_invalid_type() {
    let src = r#"
        rec Person(name: str, email: str)
        per = Person(name: "per", email: "")

        per = per.with(email: false)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'str?', found 'bool'"),
    };
}

#[test]
fn test_empty_list_not_allowed() {
    let src = r#"
        list = []
        for item in list {
            print("")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "List is always empty"),
    };
}

#[test]
fn test_empty_list_literal_not_allowed() {
    let src = r#"
        for item in [] {
            print("")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "List is always empty"),
    };
}

#[test]
fn test_expected_str_found_opt() {
    let src = r#"
        fun foo(a: str) {}

        opt = [""].find("")
        foo(opt)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'str', found 'str?'"),
    };
}

#[test]
fn test_expected_int_found_opt() {
    let src = r#"
        fun foo(a: int) {}

        opt = [0].find(0)
        foo(opt)
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'int', found 'int?'"),
    };
}

#[test]
fn test_unexpected_opt_str_in_interp() {
    let src = r#"
        opt = [""].find("")
        print("${opt}")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'str', found 'str?'"),
    };
}

#[test]
fn test_unexpected_opt_int_in_interp() {
    let src = r#"
        opt = [0].find(0)
        print("${opt}")
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => assert_eq!(err.message, "Expected 'int', found 'int?'"),
    };
}
