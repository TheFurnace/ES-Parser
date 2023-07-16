use std::{iter::repeat, ops::Index, slice::SliceIndex};
use nom::{
    bytes::complete::take_till,
    character::{
        complete::{char, tab},
        is_newline,
    },
    combinator::{all_consuming, eof, map},
    multi::{many0, many_till},
    sequence::tuple,
    IResult,
};
use crate::constants::*;

#[derive(Clone, Debug, PartialEq)]
pub struct TokenVec<'a>(pub Vec<Token<'a>>);

impl<'a> TokenVec<'a> {
    pub fn new() -> TokenVec<'a>{
        TokenVec(Vec::new())
    }
}

impl<'a> FromIterator<Token<'a>> for TokenVec<'a> {
    fn from_iter<T: IntoIterator<Item = Token<'a>>>(iter: T) -> Self {
        TokenVec(Vec::from_iter(iter))
    }
}

impl<'a, Idx> Index<Idx> for TokenVec<'a> 
where
    Idx: SliceIndex<[Token<'a>]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.0.index(index)
    }

    
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Token<'a> {
    Indent,
    Dedent,
    Value(&'a str),
}

impl<'a> Token<'a> {
    fn len(&self) -> usize {
        match self {
            Token::Indent => 0,
            Token::Dedent => 0,
            Token::Value(v) => v.len(),
        }
    }
}

/// Adds [Token::Indent] and [Token::Dedent] to facilitate parsing.
/// Adds all text except comments to [Token::Value]
pub fn tokenize<'a>(i: &'a str) -> IResult<&'a str, TokenVec> {
    let mut indent_level: usize = 0;

    let indent = |i: &'a str| -> IResult<&'a str, Vec<Token>> {
        let (rest, tabs) = many0(tab)(i)?;
        let current_indent_level = tabs.len();
        let mut result: Vec<Token> = Vec::new();

        if current_indent_level > indent_level {
            result.extend(repeat(Token::Indent).take(current_indent_level - indent_level))
        } else if current_indent_level < indent_level {
            result.extend(repeat(Token::Dedent).take(indent_level - current_indent_level))
        }

        indent_level = current_indent_level;

        Ok((rest, result))
    };

    let (rest, (mut vec, _)) = all_consuming(many_till(
        map(
            tuple((
                indent,
                map(take_till(|c| is_newline(c as u8) || c == POUND_C), |value| {
                    Token::Value(value)
                }),
                take_till(|c| is_newline(c as u8)),
                char(EOL_C),
            )),
            |(mut vec, token, _, _)| {
                if token.len() > 0 { vec.push(token) }
                vec
            },
        ),
        eof,
    ))(i)?;

    let final_dedent = Vec::from_iter(repeat(Token::Dedent).take(indent_level));
    vec.push(final_dedent);
    Ok((rest, TokenVec(vec.into_iter().flatten().collect())))
}

#[cfg(test)]
mod tests {
    use assert2::assert as assert_that;
    use super::*;

    mod tokenize {
        use super::*;

        #[test]
        fn success_one_indent() {
            let input = "\n\t\n";
            let output = tokenize(input);
            let expected_output = Ok(("", TokenVec::from_iter([Token::Indent])));

            assert_that!(output == expected_output)
        }

        #[test]
        fn success_one_indent_one_dedent() {
            let input = "\n\t\n\n";
            let output = tokenize(input);
            let expected_output = Ok(("", TokenVec::from_iter([Token::Indent, Token::Dedent])));

            assert_that!(output == expected_output)
        }

        #[test]
        fn success_comment_ignored() {
            let input = "# hello there friends\n\t\n\n";
            let output = tokenize(input);
            let expected_output = Ok(("", TokenVec::from_iter([Token::Indent, Token::Dedent])));

            assert_that!(output == expected_output)
        }

        #[test]
        fn success_value_with_indents() {
            let input = "\n\twhat a nice day\n\n";
            let output = tokenize(input);
            let expected_output = Ok(("", TokenVec::from_iter([Token::Indent, Token::Value(""), Token::Dedent])));

            assert_that!(output == expected_output)
        }
    }
}