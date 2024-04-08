use crate::{Result, Source};

/// A Monkey source code token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Token<'s> {
    Operator(Operator),
    Delimiter(Delimiter),
    Keyword(Keyword),
    Literal(Literal<'s>),
    Ident(Source<'s>),
}

impl<'s> Token<'s> {
    pub fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{branch::alt, combinator::map};

        let (new_src, tok) = alt((
            map(Operator::parse, Token::Operator),
            map(Delimiter::parse, Token::Delimiter),
            map(Keyword::parse, Token::Keyword),
            map(Literal::parse, Token::Literal),
            map(ident, Token::Ident),
        ))(src)?;

        if new_src.chars().next().is_some_and(char::is_alphabetic)
            && matches!(tok, Token::Keyword(_))
        {
            // This is _not_ a keyword, but an identifier.
            // Re-parse it as such.
            return map(ident, Token::Ident)(src);
        }

        Ok((new_src, tok))
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
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum Operator {
        EqEq => "==",
        Ne => "!=",
        Le => "<=",
        Ge => ">=",
        Eq => "=",
        Plus => "+",
        Minus => "-",
        Star => "*",
        Slash => "/",
        Bang => "!",
        Lt => "<",
        Gt => ">",
        LParen => "(",
        RParen => ")",
        LBrace => "{",
        RBrace => "}",
    }
}

symbol_tok! {
    /// A Monkey delimiter.
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum Delimiter {
        Comma => ",",
        Semi => ";",
    }
}

symbol_tok! {
    /// A Monkey keyword.
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum Keyword {
        Fn => "fn",
        Let => "let",
        True => "true",
        False => "false",
        If => "if",
        Else => "else",
        Return => "return",
    }
}

/// A Monkey literal.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Literal<'s> {
    Integer(u64),
    String(Source<'s>),
}

impl<'s> Literal<'s> {
    fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_till},
            character::complete::u64,
            combinator::map,
            sequence::delimited,
        };

        alt((
            map(u64, Literal::Integer),
            map(
                delimited(tag("\""), take_till(|ch| ch == '"'), tag("\"")),
                Literal::String,
            ), // other
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

    // Identifiers may only start with `_` or
    // alphabetic characters.

    let first = src
        .chars()
        .next()
        .ok_or_else(|| Err::Failure(make_error(src, ErrorKind::TakeWhile1)))?;

    if !(first == '_' || first.is_alphabetic()) {
        return Err(Err::Failure(make_error(src, ErrorKind::Tag)));
    }

    // Naming rule is satisfied, taking freely
    take_while1(|ch: char| ch == '_' || ch.is_alphanumeric())(src)
}

#[cfg(test)]
mod tests {
    use super::{Keyword, Literal, Operator, Token, Delimiter};

    #[test]
    fn token() {
        assert_eq!(
            Token::parse("+").unwrap().1,
            Token::Operator(Operator::Plus)
        );
        assert_eq!(
            Token::parse(";").unwrap().1,
            Token::Delimiter(Delimiter::Semi),
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
    fn delimiter() {
        assert_eq!(Delimiter::parse(",").unwrap().1, Delimiter::Comma);
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
        assert_eq!(
            Literal::parse(r#""hello, world!""#).unwrap().1,
            Literal::String("hello, world!")
        )
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
