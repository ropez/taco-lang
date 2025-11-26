use taco::check_output;

#[test]
fn test_simple_enum() {
    let src = r#"
        enum Hero(
            Fred
            Wilma
            Barney
        )

        hero = Hero::Fred

        print("$hero")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Fred", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_comparison() {
    let src = r#"
        enum Hero(
            Fred
            Wilma
            Barney
        )

        hero = Hero::Fred

        println("${hero == Hero::Fred}")
        println("${hero == Hero::Wilma}")
        println("${Hero::Wilma == Hero::Barney}")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("true\nfalse\nfalse\n", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn test_comparison_not_allowed_for_different_enums() {
    let src = r#"
        enum Hero (Fred, Barney)
        enum Villain(Vandercave, Butterbean)

        if Hero::Fred != Villain::Butterbean {
            print("poop")
        }
    "#;

    match check_output(src) {
        Ok(_) => panic!("Expected error"),
        Err(err) => {
            println!("{err}");
            assert_eq!(err.message, "Expected Hero, found Villain")
        },
    };
}

#[test]
fn test_enum_with_value() {
    let src = r#"
        enum Country (
            Spain
            France
            Italy
        )

        enum City (
            Paris
            London
            Madrid
        )

        enum VacationPlan (
            StayAtHome
            TravelTo(Country, City)
        )

        plan = VacationPlan::TravelTo(Country::Spain, City::Madrid)
        print("$plan")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("TravelTo(Spain, Madrid)", out),
        Err(err) => panic!("{err}"),
    };
}

#[test]
fn fails_for_missing_value() {
    let src = r#"
        enum City (
            Paris
            London
            Madrid
        )

        enum VacationPlan (
            StayAtHome
            TravelTo(City)
        )

        fun foo(plan: VacationPlan) {
            print("$plan")
        }

        foo(VacationPlan::TravelTo)
    "#;

    match check_output(src) {
        Ok(out) => panic!("Expected error, got {out}"),
        Err(err) => assert_eq!(err.message, "Expected VacationPlan got VacationPlan::TravelTo"),
    };
}

#[test]
fn fails_for_unexpected_call() {
    let src = r#"
        enum City (
            Paris
            London
            Madrid
        )

        enum VacationPlan (
            StayAtHome
            TravelTo(City)
        )

        fun foo(plan: VacationPlan) {
            print("$plan")
        }

        foo(VacationPlan::StayAtHome())
    "#;

    match check_output(src) {
        Ok(out) => panic!("Expected error, got {out}"),
        Err(err) => assert_eq!(err.message, "Variant not callable: VacationPlan::StayAtHome"),
    };
}

#[test]
fn test_apply_arguments_from_tuple() {
    let src = r#"
        enum Receiver(
            Person(name: str, age: int)
            Group(name: str)
        )

        args = ("per", 42)
        per = Receiver::Person(=args)

        print("$per")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "Person(name: per, age: 42)"),
        Err(err) => panic!("{err}"),
    };

}

#[test]
fn test_pass_variant_as_function() {
    let src = r#"
        enum Receiver(
            Person(name: str, age: int)
            Group(name: str)
        )

        args = [
            ("per", 42)
            ("kari", 36)
        ]

        people = args.map_to(Receiver::Person)

        print("$people")
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!(out, "[Person(name: per, age: 42), Person(name: kari, age: 36)]"),
        Err(err) => panic!("{err}"),
    };

}

#[test]
fn test_apply_params() {
    let src = r#"
        enum Enum(
            Bar
            Foo(a: int, b: int, c: int, d: int)
        )

        fun dump(foo: Enum) {
            println("$foo")
        }

        dump(Enum::Foo(1, 2, 3, 4))
        dump(Enum::Foo(1, 2, 3, d: 4))
        dump(Enum::Foo(1, 2, d: 4, c: 3))
        dump(Enum::Foo(1, d: 4, c: 3, b: 2))
        dump(Enum::Foo(c: 3, d: 4, a: 1, b: 2))
        dump(Enum::Foo(b: 2, c: 3, d: 4, 1))
        dump(Enum::Foo(b: 2, 1, c: 3, d: 4))
        dump(Enum::Foo(d: 4, 1, c: 3, 2))
        dump(Enum::Foo(1, d: 4, 2, 3))
        dump(Enum::Foo(1, c: 3, 2, 4))
    "#;

    match check_output(src) {
        Ok(out) => assert_eq!("Foo(a: 1, b: 2, c: 3, d: 4)\n".repeat(10), out),
        Err(err) => panic!("{err}"),
    }
}

