use crate::{constants::*, lexer::*, subvalue::*};
use indexmap::IndexMap;
use nom::{
    branch::alt,
    bytes::complete::{take_till, take_while1},
    character::complete::char,
    combinator::{all_consuming, fail, map, opt, eof},
    multi::{many1, many0},
    sequence::tuple,
    IResult, Parser, error::{make_error, ErrorKind},
};

pub fn parse(i: &str) -> SubValue {
    let (_, tokens) = match tokenize(i) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Failed to tokenize input with error:\n{}", e);
            ("", TokenVec::new())
        }
    };

    // let result = many0(alt((subvalue_object, subvalue_list, subvalue_single)))();
    let result = all_consuming(many0(token_object))(&tokens[..]);
    match result {
        Ok((_, r)) => organize_children(r),
        Err(e) => {
            eprint!("OH NO\n{:?}", e);
            panic!("Help!")
        },
    }
}

/* Parse SubValues from Tokens */

fn parse_tokens<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], (String, SubValue)> {
    alt((token_object, token_inline_object, token_single))(i)
}

/// Matches a value token with 2 words separated by a space.
/// #### Caution: Will match [token_list]
fn token_single<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], (String, SubValue)> {
    map(
        token_value(tuple((word, opt(tuple((char(SPACE_C), word)))))),
        |(key, optional_value)| {
            match optional_value {
                Some((_, value)) => (key.to_string(), SubValue::from(value)),
                None => (key.to_string(), SubValue::from("true")),
            }
        },
    )(i)
}

fn token_inline_object<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], (String, SubValue)> {
    map(
        token_value(tuple((word, char(SPACE_C), word, char(SPACE_C), word))),
        |(key, _, name, _, count)| {
            (
                key.to_string(),
                SubValue::from_iter([
                    (String::from("name"), SubValue::from(name)),
                    (String::from("count"), SubValue::from(count)),
                ]),
            )
        },
    )(i)
}

fn token_object<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], (String, SubValue)> {
    map(
        tuple((
            token_value(tuple((word, opt(tuple((char(SPACE_C), word)))))),
            token_indent,
            many1(parse_tokens),
            token_dedent,
        )),
        |((key, name_option), _, mut children, _)| {
            let mut v: Vec<(String, SubValue)> = Vec::new();
            if let Some((_, name)) = name_option {
                v.push((String::from("name"), SubValue::from(name)));
            }
            v.append(&mut children);
            (key.to_string(), organize_children(v))
        },
    )(i)
}

/* Parse SubValues from strings */

fn word(i: &str) -> IResult<&str, &str> {
    alt((unquoted_word, quoted_word))(i)
}

fn unquoted_word(i: &str) -> IResult<&str, &str> {
    take_while1(|c| c != SPACE_C && c != QUOTE_C)(i)
}

fn quoted_word(i: &str) -> IResult<&str, &str> {
    map(
        tuple((char(QUOTE_C), take_till(|c| c == QUOTE_C), char(QUOTE_C))),
        |(_, v, _)| v,
    )(i)
}

/* helpers */

fn organize_children(i: Vec<(String, SubValue)>) -> SubValue {
    let mut d: IndexMap<String, SubValue> = IndexMap::new();

    i.into_iter().for_each(|(key, value): (String, SubValue)| {
        if d.contains_key(&key) {
            let old_v = d.remove(&key).unwrap();
            match old_v {
                SubValue::Single(_) | SubValue::Object(_) => {
                    let new_v: Vec<SubValue> = Vec::from([old_v, value]);
                    d.insert(key, SubValue::List(new_v));
                }
                SubValue::List(mut l) => {
                    l.push(value);
                    d.insert(key, SubValue::List(l));
                }
            }
        } else {
            d.insert(key, value);
        }
    });

    SubValue::Object(d)
}

/// Match if the token is [Token::Value] and the parser accepts the internal value.
fn token_value<'a, O, E, F>(mut f: F) -> impl FnMut(&'a [Token<'a>]) -> IResult<&'a [Token<'a>], O>
where
    F: Parser<&'a str, O, E>,
{
    move |i: &[Token]| match i.get(0) {
        Some(token) => match token {
            Token::Indent => fail(i),
            Token::Dedent => fail(i),
            Token::Value(v) => match f.parse(v) {
                Ok(r) => Ok((&i[1..], r.1)),
                Err(_) => fail(i),
            },
        },
        None => Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Eof))),
    }
    
}

/// Match if the token is [Token::Indent].
fn token_indent<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Token<'a>> {
    match i.get(0) {
        Some(token) => {
            match token {
                Token::Indent => Ok((&i[1..], i[0])),
                Token::Dedent => fail(i),
                Token::Value(_) => fail(i),
            }
        },
        None => Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Eof))),
    }
}

/// Match if the token is [Token::Indent].
fn token_dedent<'a>(i: &'a [Token]) -> IResult<&'a [Token<'a>], Token<'a>> {
    match i.get(0) {
        Some(token) => {
            match token {
                Token::Indent => fail(i),
                Token::Dedent => Ok((&i[1..], i[0])),
                Token::Value(_) => fail(i),
            }
        },
        None => Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Eof))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert2::assert as assert_that;
    use assert2::check;
    use assert2::let_assert;
    use nom::error::ErrorKind;
    use test_case::case;

    mod unquoted_word {
        use super::*;

        #[test]
        fn success_single_word() {
            let input = "hello";
            let output = unquoted_word(input);
            let expected_output = Ok(("", "hello"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_word_in_sentence() {
            let input = "hello how are you";
            let output = unquoted_word(input);
            let expected_output = Ok((" how are you", "hello"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_starts_with_space() {
            let input = " hello how are you";
            let output = unquoted_word(input);
            let expected_output = nom_error(" hello how are you", ErrorKind::TakeWhile1);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_is_quoted() {
            let input = "\"hello how are you";
            let output = unquoted_word(input);
            let expected_output = nom_error("\"hello how are you", ErrorKind::TakeWhile1);

            assert_that!(output == expected_output);
        }
    }

    mod quoted_word {
        use super::*;

        #[test]
        fn success_single_quoted_word() {
            let input = "\"hello\"";
            let output = quoted_word(input);
            let expected_output = Ok(("", "hello"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_multiple_quoted_words() {
            let input = "\"hello world\"";
            let output = quoted_word(input);
            let expected_output = Ok(("", "hello world"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_multiple_quoted_words_in_sentence() {
            let input = "\"hello world\" \"goodbye\"";
            let output = quoted_word(input);
            let expected_output = Ok((" \"goodbye\"", "hello world"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_starts_with_space() {
            let input = " \"hello how are you\"";
            let output = quoted_word(input);
            let expected_output = nom_error(" \"hello how are you\"", ErrorKind::Char);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_missing_start_quote() {
            let input = "hello world\"";
            let output = quoted_word(input);
            let expected_output = nom_error("hello world\"", ErrorKind::Char);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_missing_end_quote() {
            let input = "\"hello world";
            let output = quoted_word(input);
            let expected_output = nom_error("", ErrorKind::Char);

            assert_that!(output == expected_output);
        }
    }

    mod word {
        use super::*;

        #[test]
        fn success_unquoted_word() {
            let input = "hello";
            let output = word(input);
            let expected_output = Ok(("", "hello"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_quoted_word() {
            let input = "\"hello world\"";
            let output = word(input);
            let expected_output = Ok(("", "hello world"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_starts_with_space() {
            let input = " hello how are you";
            let output = word(input);
            let expected_output = nom_error(" hello how are you", ErrorKind::Char);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_missing_end_quote() {
            let input = "\"hello world";
            let output = word(input);
            let expected_output = nom_error("", ErrorKind::Char);

            assert_that!(output == expected_output);
        }
    }

    mod token_value {
        use super::*;

        #[test]
        fn success_token_value_with_word() {
            let input = [Token::Value("hello")];
            let output = token_value(word)(&input);
            let expected_output: IResult<&[Token], &str> = Ok((&[], "hello"));

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_token_value_with_failing_word() {
            let input = [Token::Value(" hello")];
            let output = token_value(word)(&input);
            let expected_output: IResult<&[Token], &str> = nom_error(&input, ErrorKind::Fail);

            assert_that!(output == expected_output);
        }

        #[case(Token::Indent ; "Indent")]
        #[case(Token::Dedent ; "Dedent")]
        fn fail_token(i: Token) {
            let input = [i];
            let output = token_value(word)(&input);
            let expected_output: IResult<&[Token], &str> = nom_error(&input, ErrorKind::Fail);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_eof(){
            let input: [Token; 0] = [];
            let output = token_value(word)(&input);
            let expected_output: IResult<&[Token], &str> = nom_error(&input, ErrorKind::Eof);

            assert_that!(output == expected_output);
        }
    }

    mod token_indent {
        use super::*;

        #[test]
        fn success_token_indent() {
            let input = TokenVec::from_iter([Token::Indent]);
            let output = token_indent(&input[..]);
            let expected_output: IResult<&[Token], Token> = Ok((&[], Token::Indent));

            assert_that!(output == expected_output);
        }

        #[case(Token::Value("hello") ; "Value")]
        #[case(Token::Dedent ; "Dedent")]
        fn fail_token(i: Token) {
            let input = TokenVec::from_iter([i]);
            let output = token_indent(&input[..]);
            let expected_output = nom_error(&input[..], ErrorKind::Fail);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_eof(){
            let input: [Token; 0] = [];
            let output = token_indent(&input);
            let expected_output: IResult<&[Token], Token> = nom_error(&input, ErrorKind::Eof);

            assert_that!(output == expected_output);
        }
    }

    mod token_dedent {
        use super::*;

        #[test]
        fn success_token_dedent() {
            let input = TokenVec::from_iter([Token::Dedent]);
            let output = token_dedent(&input[..]);
            let expected_output: IResult<&[Token], Token> = Ok((&[], Token::Dedent));

            assert_that!(output == expected_output);
        }

        #[case(Token::Value("hello") ; "Value")]
        #[case(Token::Indent ; "Indent")]
        fn fail_token(i: Token) {
            let input = TokenVec::from_iter([i]);
            let output = token_dedent(&input[..]);
            let expected_output = nom_error(&input[..], ErrorKind::Fail);

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_eof(){
            let input: [Token; 0] = [];
            let output = token_dedent(&input);
            let expected_output: IResult<&[Token], Token> = nom_error(&input, ErrorKind::Eof);

            assert_that!(output == expected_output);
        }
    }

    mod organize_children {
        use super::*;

        #[test]
        fn success_simple() {
            let input = Vec::from_iter([
                (String::from("single"), SubValue::from("world")),
                (
                    String::from("list"),
                    SubValue::from_iter([SubValue::from("list_value")]),
                ),
                (
                    String::from("object"),
                    SubValue::from_iter([(
                        String::from("inner object"),
                        SubValue::from("object value"),
                    )]),
                ),
                (String::from("adhoc_list"), SubValue::from("hello")),
                (String::from("adhoc_list"), SubValue::from("world")),
                (String::from("adhoc_list"), SubValue::from("three")),
            ]);

            let output = organize_children(input);

            let_assert!(SubValue::Object(dictionary) = output);

            let contains_key_single = dictionary.contains_key("single");
            check!(contains_key_single);
            if contains_key_single {
                let_assert!(SubValue::Single(single_value) = &dictionary["single"]);
                let expected_single_value = "world";
                check!(single_value == expected_single_value);
            }

            let contains_key_list = dictionary.contains_key("list");
            check!(contains_key_list);
            if contains_key_list {
                let_assert!(SubValue::List(list_value) = &dictionary["list"]);
                let expected_list_value = &Vec::from_iter([SubValue::from("list_value")]);
                check!(list_value == expected_list_value);
            }

            let contains_key_object = dictionary.contains_key("object");
            check!(contains_key_object);
            if contains_key_object {
                let_assert!(SubValue::Object(object_value) = &dictionary["object"]);
                let expected_object_value: &IndexMap<String, SubValue> = &IndexMap::from_iter([(
                    String::from("inner object"),
                    SubValue::from("object value"),
                )]);
                check!(object_value == expected_object_value);
            }

            let contains_key_adhoc_list = dictionary.contains_key("adhoc_list");
            check!(contains_key_adhoc_list);
            if contains_key_list {
                let_assert!(SubValue::List(list_value) = &dictionary["adhoc_list"]);
                let expected_adhoc_list = &Vec::from_iter([
                    SubValue::from("hello"),
                    SubValue::from("world"),
                    SubValue::from("three"),
                ]);
                check!(list_value == expected_adhoc_list);
            }
        }
    }

    mod token_single {
        use super::*;

        #[test]
        fn success_unquoted_word() {
            let input = [Token::Value("hello")];
            let output = token_single(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> =
                Ok((&[], (String::from("hello"), SubValue::from("true"))));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_unquoted_word_pair() {
            let input = [Token::Value("hello world")];
            let output = token_single(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> =
                Ok((&[], (String::from("hello"), SubValue::from("world"))));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_quoted_word_pair() {
            let input = [Token::Value("\"hello world\" \"and goodbye\"")];
            let output = token_single(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> = Ok((
                &[],
                (String::from("hello world"), SubValue::from("and goodbye")),
            ));

            assert_that!(output == expected_output);
        }

        #[test]
        fn success_mixed_word_pair() {
            let input = [Token::Value("\"hello world\" goodbye")];
            let output = token_single(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> = Ok((
                &[],
                (String::from("hello world"), SubValue::from("goodbye")),
            ));

            assert_that!(output == expected_output);
        }
    }

    mod token_inline_object {
        use super::*;
        #[test]
        fn success_word_triple() {
            let input = [Token::Value("hello world, \"and goodbye\"")];
            let output = token_inline_object(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> = Ok((
                &[],
                (
                    String::from("hello"),
                    SubValue::from_iter([
                        (String::from("name"), SubValue::from("world,")),
                        (String::from("count"), SubValue::from("and goodbye")),
                    ]),
                ),
            ));

            assert_that!(output == expected_output);
        }

        #[test]
        fn fail_word_pair() {
            let input = [Token::Value("\"hello world\" \"and goodbye\"")];
            let output = token_inline_object(&input);
            let expected_output: IResult<&[Token], (String, SubValue)> =
                nom_error(&input, ErrorKind::Fail);

            assert_that!(output == expected_output);
        }
    }

    mod token_object {
        use super::*;

        #[test]
        fn success_simple_object() {
            let input = [
                Token::Value("object_key object_name"),
                Token::Indent,
                Token::Value("single \"single value\""),
                Token::Value("inline_object my_name my_count"),
                Token::Dedent,
            ];
            let output = token_object(&input);

            let_assert!(Ok((remaining_input, (key, value))) = output);

            let expected_remaining_input: &[Token; 0] = &[];
            check!(remaining_input == expected_remaining_input);

            let expected_key = String::from("object_key");
            check!(key == expected_key);

            let_assert!(SubValue::Object(object) = value);

            let contains_name_key = object.contains_key("name");
            check!(contains_name_key);
            if contains_name_key {
                let_assert!(SubValue::Single(name_value) = &object["name"]);
                let expected_name_value = &String::from("object_name");
                check!(name_value == expected_name_value);
            }

            let contains_single_key = object.contains_key("single");
            check!(contains_single_key);
            if contains_single_key {
                let_assert!(SubValue::Single(single_value) = &object["single"]);
                let expected_single_value = &String::from("single value");
                check!(single_value == expected_single_value);
            }

            let contains_inline_object_key = object.contains_key("inline_object");
            check!(contains_inline_object_key);
            if contains_inline_object_key {
                let_assert!(SubValue::Object(inline_object_value) = &object["inline_object"]);
                let expected_object_value: &IndexMap<String, SubValue> = &IndexMap::from_iter([
                    (String::from("name"), SubValue::from("my_name")),
                    (String::from("count"), SubValue::from("my_count")),
                ]);
                check!(inline_object_value == expected_object_value);
            }
        }

        #[test]
        fn success_nested_object() {
            let input = [
                Token::Value("object_key object_name"),
                Token::Indent,
                Token::Value("single \"single value\""),
                Token::Value("inner_object \"inner_object_name\""),
                Token::Indent,
                Token::Value("entry1 \"entry1 value\""),
                Token::Value("entry2 \"entry2 value\""),
                Token::Dedent,
                Token::Value("inline_object my_name my_count"),
                Token::Dedent,
            ];
            let output = token_object(&input);

            let_assert!(Ok((remaining_input, (key, value))) = output);

            let expected_remaining_input: &[Token; 0] = &[];
            check!(remaining_input == expected_remaining_input);

            let expected_key = String::from("object_key");
            check!(key == expected_key);

            let_assert!(SubValue::Object(object) = value);

            let contains_inner_object_key = object.contains_key("inner_object");
            check!(contains_inner_object_key);
            if contains_inner_object_key {
                let_assert!(SubValue::Object(inner_object_value) = &object["inner_object"]);
                let expected_object_value: &IndexMap<String, SubValue> = &IndexMap::from_iter([
                    (String::from("name"), SubValue::from("inner_object_name")),
                    (String::from("entry1"), SubValue::from("entry1 value")),
                    (String::from("entry2"), SubValue::from("entry2 value")),
                ]);
                check!(inner_object_value == expected_object_value);
            }
        }
    }

    fn nom_error<I, O>(i: I, err: ErrorKind) -> IResult<I, O> {
        Err(nom::Err::Error(nom::error::Error::new(i, err)))
    }
}
