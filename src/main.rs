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

        fruits = [
            "apple"
            "orange"
            "banana"
        ]

        for fruit in fruits {
            println(fruit)
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
