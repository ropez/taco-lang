use crate::interpolation::{StringToken, tokenise_string};

#[test]
fn test_returns_empty_string() {
    let res = tokenise_string("");

    assert_eq!(res, vec![]);
}

#[test]
fn test_returns_whole_string() {
    let res = tokenise_string("foobar");

    assert_eq!(res, vec![StringToken::string("foobar", 0)]);
}

#[test]
fn test_returns_string_with_dollar_signs() {
    let res = tokenise_string("$$foo$$bar$$");

    assert_eq!(
        res,
        vec![
            StringToken::string("$foo", 0),
            StringToken::string("$bar", 5),
            StringToken::string("$", 10),
        ]
    );
}

#[test]
fn test_returns_string_with_escapes() {
    let res = tokenise_string(r#"\\foo\r\nbar\r\n"#);

    assert_eq!(
        res,
        vec![
            StringToken::string("\\foo", 0),
            StringToken::string("\r", 5),
            StringToken::string("\n", 7),
            StringToken::string("bar", 9),
            StringToken::string("\r", 12),
            StringToken::string("\n", 14),
        ]
    );
}

#[test]
fn test_returns_single_expression() {
    let res = tokenise_string("$foobar");

    assert_eq!(res, vec![StringToken::expr("foobar", 1)]);
}

#[test]
fn test_returns_multiple_expressions() {
    let res = tokenise_string("$foo$bar");

    assert_eq!(
        res,
        vec![StringToken::expr("foo", 1), StringToken::expr("bar", 5)]
    );
}

#[test]
fn test_returns_string_and_then_expression() {
    let res = tokenise_string("√ Item: $foo");

    assert_eq!(
        res,
        vec![
            StringToken::string("√ Item: ", 0),
            StringToken::expr("foo", 11)
        ]
    );
}

#[test]
fn test_returns_expression_and_then_string() {
    let res = tokenise_string("$foo is the item √");

    assert_eq!(
        res,
        vec![
            StringToken::expr("foo", 1),
            StringToken::string(" is the item √", 4)
        ]
    );
}

#[test]
fn test_returns_expression_followed_by_punctuation() {
    let res = tokenise_string("$foo, said the troll");

    assert_eq!(
        res,
        vec![
            StringToken::expr("foo", 1),
            StringToken::string(", said the troll", 4),
        ]
    );
}

#[test]
fn test_returns_expression_inside_braces() {
    let res = tokenise_string("foo${koko + foo.item()}bar");

    assert_eq!(
        res,
        vec![
            StringToken::string("foo", 0),
            StringToken::expr("koko + foo.item()", 5),
            StringToken::string("bar", 23),
        ]
    );
}

#[test]
fn test_returns_expression_inside_braces_with_non_ascii() {
    let res = tokenise_string("√ Item: ${foo.item()}--√");

    assert_eq!(
        res,
        vec![
            StringToken::string("√ Item: ", 0),
            StringToken::expr("foo.item()", 12),
            StringToken::string("--√", 23),
        ]
    );
}
