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

        fun explain(a, b) {
            println(a, b)
        }

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
            explain("--", fruit)
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
