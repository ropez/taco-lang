use parser::AstNode;

mod lexer;
mod parser;

fn main() {
    let code = r#"
        println("Hello, world!")
    "#;
    let tokens = lexer::tokenize(code);
    let ast = parser::parse(tokens);

    for node in ast {
        match node {
            AstNode::Call { subject, arguments } => match subject.as_ref() {
                "println" => {
                    for arg in arguments {
                        println!("{}", arg);
                    }
                }
                _ => panic!("Undefined function: {subject:?}"),
            },
        }
    }
}
