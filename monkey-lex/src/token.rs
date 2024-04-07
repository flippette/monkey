use crate::{Result, Source};

/// A Monkey source code token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Token<'s> {
    /// Operator.
    Operator(Operator),
    /// Keyword.
    Keyword(Keyword),
    /// Literal.
    Literal(Literal),
    /// Identifier.
    Ident(Source<'s>),
}

impl<'s> Token<'s> {
    pub fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{branch::alt, combinator::map};

        alt((
            map(Operator::parse, Token::Operator),
            map(Keyword::parse, Token::Keyword),
            map(Literal::parse, Token::Literal),
            map(ident, Token::Ident),
        ))(src)
    }
}

/// Defines a symbol-only (fieldless) token type
/// and an accompanying `parse` function.
///
/// Longer repetitions must go first to avoid
/// ambiguity (like how [`Operator::EqEq`] comes before
/// [`Operator::Eq`], for example).
macro_rules! symbol_tok {
    (
        $(#[$outer:meta])*
        $vis:vis enum $name:ident {
            $($variant:ident => $symbol:literal),+ $(,)?
        }
    ) => {
        $(#[$outer])*
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        $vis enum $name {
            $(
                #[doc = concat!("`", $symbol, "`")]
                $variant,
            )+
        }

        impl $name {
            fn parse(src: $crate::Source) -> $crate::Result<Self> {
                ::nom::branch::alt((
                    $(::nom::combinator::map(
                        ::nom::bytes::complete::tag($symbol),
                        |_| $name::$variant
                    ),)+
                ))(src)
            }
        }
    };
}

symbol_tok! {
    /// A Monkey operator.
    pub enum Operator {
        EqEq => "==",
        Eq => "=",
        Plus => "+",
        Minus => "-",
        Star => "*",
        Slash => "/",
        LParen => "(",
        RParen => ")",
        LBrace => "{",
        RBrace => "}",
        Comma => ",",
        Semi => ";",
    }
}

symbol_tok! {
    /// A Monkey keyword.
    pub enum Keyword {
        Fn => "fn",
        Let => "let",
    }
}

/// A Monkey literal.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Literal {
    Integer(u64),
}

impl Literal {
    fn parse(src: Source) -> Result<Self> {
        use nom::{branch::alt, character::complete::u64, combinator::map};

        alt((
            map(u64, Literal::Integer),
            // other
        ))(src)
    }
}

/// Parse an identifier from the beginning of a source slice.
///
/// There's no `Identifier` struct so this is a standalone
/// function.
fn ident(src: Source) -> Result<Source> {
    use nom::{
        bytes::complete::take_while1,
        error::{make_error, ErrorKind},
        Err,
    };

    let first = src
        .chars()
        .next()
        .ok_or_else(|| Err::Failure(make_error(src, ErrorKind::TakeWhile1)))?;

    if !(first == '_' || first.is_alphabetic()) {
        return Err(Err::Failure(make_error(src, ErrorKind::Tag)));
    }

    take_while1(|ch: char| ch == '_' || ch.is_alphanumeric())(src)
}

#[cfg(test)]
mod tests {
    use super::{Keyword, Literal, Operator, Token};

    #[test]
    fn token() {
        assert_eq!(
            Token::parse("+").unwrap().1,
            Token::Operator(Operator::Plus)
        );
        assert_eq!(Token::parse("fn").unwrap().1, Token::Keyword(Keyword::Fn));
        assert_eq!(
            Token::parse("123").unwrap().1,
            Token::Literal(Literal::Integer(123))
        );
    }

    #[test]
    fn operator() {
        assert_eq!(Operator::parse("==").unwrap().1, Operator::EqEq);
    }

    #[test]
    fn keyword() {
        assert_eq!(Keyword::parse("fn").unwrap().1, Keyword::Fn);
    }

    #[test]
    fn literal() {
        assert_eq!(
            Literal::parse("816723").unwrap().1,
            Literal::Integer(816723)
        );
    }

    #[test]
    fn ident() {
        assert_eq!(
            super::ident("__some_ident123").unwrap().1,
            "__some_ident123",
        );
        assert!(super::ident("1_invalid_ident").is_err());
    }
}
