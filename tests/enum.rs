use taco::check_output;

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

