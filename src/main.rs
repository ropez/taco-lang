mod lexer;
mod parser;
mod eval;

fn main() {
    let code = r#"
        msg = "What's up?"
        println(
            "Hello, world!"
            msg
        )
        println(msg)

        prefix = "--"
        fun explain(a) {
            println(prefix, a)
        }
        prefix = "not this"

        banana = "banana"
        fruits = [
            "apple"
            "orange"
            banana
        ]

        fruits = push(
            fruits
            "pineapple"
        )

        for fruit in fruits {
            explain(fruit)
        }

        fun bye() {
            msg = "Good bye"
            println(msg)
        }

        bye()
    "#;

    let tokens = lexer::tokenize(code);
    let ast = parser::parse(tokens);

    eval::eval(&ast);
}
