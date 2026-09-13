use crate::{
    ident::Ident,
    lexer::{QuotationKind, TokenKind, Tokenizer},
};

use QuotationKind::*;

struct Wrap<'a> {
    src: &'a str,
    pub iter: Tokenizer<'a>,
}

impl<'a> Wrap<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            iter: Tokenizer::new(src),
        }
    }

    fn next(&mut self) -> TokenKind {
        match self
            .iter
            .next_token()
            .map_err(|err| err.into_source_error(self.src))
            .unwrap()
        {
            Some(t) => t.cloned(),
            None => panic!("Expected token"),
        }
    }

    fn chars(&mut self, quotation: QuotationKind) -> &'a str {
        self.iter
            .next_string_chars(&quotation)
            .map(|k| k.into_inner())
            .map_err(|err| err.into_source_error(self.src))
            .unwrap()
    }
}

#[test]
fn test_tokenize_empty_string() {
    let src = r#"s = """#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Identifier(Ident::from("s")));
    assert_eq!(tokens.next(), TokenKind::Assign);
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(QuotationKind::DoubleQuote), "");
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
}

#[test]
fn test_tokenize_simple_string() {
    let src = r#"s = "hello""#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Identifier(Ident::from("s")));
    assert_eq!(tokens.next(), TokenKind::Assign);
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "hello");
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
}

#[test]
fn test_tokenize_verbatim_string() {
    let src = r#""""{"name": "Taco"}""""#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Quotation(TripleQuotes));
    assert_eq!(tokens.chars(TripleQuotes), r#"{"name": "Taco"}"#);
    assert_eq!(tokens.next(), TokenKind::Quotation(TripleQuotes));
}

#[test]
fn test_tokenize_string_with_dollar_signs() {
    let src = r#""$$foo$$bar$$""#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "");
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.chars(DoubleQuote), "foo");
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.chars(DoubleQuote), "bar");
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
}

#[test]
fn test_returns_string_with_escapes() {
    let src = r#""\\foo\r\nbar\r\n""#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "");
    assert_eq!(tokens.next(), TokenKind::Escape('\\'));
    assert_eq!(tokens.chars(DoubleQuote), "foo");
    assert_eq!(tokens.next(), TokenKind::Escape('r'));
    assert_eq!(tokens.next(), TokenKind::Escape('n'));
    assert_eq!(tokens.chars(DoubleQuote), "bar");
    assert_eq!(tokens.next(), TokenKind::Escape('r'));
    assert_eq!(tokens.next(), TokenKind::Escape('n'));
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
}

#[test]
fn test_tokenize_interpolated_string() {
    let src = r#""{ hello } $foo ${ world + "{}" }" + "!""#;

    let mut tokens = Wrap::new(src);

    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "{ hello } ");
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::Identifier(Ident::from("foo")));
    assert_eq!(tokens.chars(DoubleQuote), " ");
    assert_eq!(tokens.next(), TokenKind::Dollar);
    assert_eq!(tokens.next(), TokenKind::LeftBrace);
    assert_eq!(tokens.next(), TokenKind::Identifier(Ident::from("world")));
    assert_eq!(tokens.next(), TokenKind::Plus);
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "{}");
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.next(), TokenKind::RightBrace);
    assert_eq!(tokens.chars(DoubleQuote), "");
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));

    assert_eq!(tokens.next(), TokenKind::Plus);
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
    assert_eq!(tokens.chars(DoubleQuote), "!");
    assert_eq!(tokens.next(), TokenKind::Quotation(DoubleQuote));
}
