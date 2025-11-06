#![allow(unused)]

#[derive(Debug, PartialEq)]
pub enum StringToken<'a> {
    Str(&'a str),
    Expr(&'a str),
}

pub fn tokenise_string<'a>(src: &'a str) -> Vec<StringToken<'a>> {
    let mut tokens = Vec::new();

    let mut cur = src;
    while !cur.is_empty() {
        if let Some((l, h)) = cur.split_once('$') {
            if !l.is_empty() {
                tokens.push(StringToken::Str(l));
            }

            cur = h;

            match cur.chars().next() {
                None => panic!("Unexpected end of string"),
                Some('A'..='Z' | 'a'..='z') => {
                    if let Some(pos) = cur.find(|ch: char| !ch.is_ascii_alphanumeric()) {
                        let (l, h) = cur.split_at(pos);
                        tokens.push(StringToken::Expr(l));

                        cur = h;
                    } else {
                        tokens.push(StringToken::Expr(cur));
                        break;
                    }
                }
                Some('{') => {
                    // XXX Support nested blocks
                    if let Some(pos) = cur.find('}') {
                        let (l, h) = cur.split_at(pos + 1);
                        tokens.push(StringToken::Expr(l.trim_matches(['{', '}'])));

                        cur = h;
                    } else {
                        panic!("Unterminated block in string")
                    }
                }
                Some('$') => {
                    todo!();
                }
                Some(_) => panic!("Unespected character after $"),
            }
        } else {
            tokens.push(StringToken::Str(cur));
            break;
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_returns_empty_string() {
        let res = tokenise_string("");

        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_returns_whole_string() {
        let res = tokenise_string("foobar");

        assert_eq!(res, vec![StringToken::Str("foobar")]);
    }

    #[test]
    fn test_returns_single_expression() {
        let res = tokenise_string("$foobar");

        assert_eq!(res, vec![StringToken::Expr("foobar")]);
    }

    #[test]
    fn test_returns_multiple_expressions() {
        let res = tokenise_string("$foo$bar");

        assert_eq!(
            res,
            vec![StringToken::Expr("foo"), StringToken::Expr("bar")]
        );
    }

    #[test]
    fn test_returns_string_and_then_expression() {
        let res = tokenise_string("√ Item: $foo");

        assert_eq!(
            res,
            vec![StringToken::Str("√ Item: "), StringToken::Expr("foo")]
        );
    }

    #[test]
    fn test_returns_expression_and_then_string() {
        let res = tokenise_string("$foo is the item √");

        assert_eq!(
            res,
            vec![StringToken::Expr("foo"), StringToken::Str(" is the item √")]
        );
    }

    #[test]
    fn test_returns_expression_followed_by_punctuation() {
        let res = tokenise_string("$foo, said the troll");

        assert_eq!(
            res,
            vec![
                StringToken::Expr("foo"),
                StringToken::Str(", said the troll"),
            ]
        );
    }

    #[test]
    fn test_returns_expression_inside_braces() {
        let res = tokenise_string("√ Item: ${foo.item()}--√");

        assert_eq!(
            res,
            vec![
                StringToken::Str("√ Item: "),
                StringToken::Expr("foo.item()"),
                StringToken::Str("--√"),
            ]
        );
    }
}
