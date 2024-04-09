use crate::{Result, Source};

/// A Monkey source code token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Token<'s> {
    Operator(Operator),
    Delimiter(Delimiter),
    Keyword(Keyword),
    Literal(Literal<'s>),
    Ident(Ident<'s>),
}

impl<'s> Token<'s> {
    /// Parse a [`Token`] from the beginning of a [`Source`] slice.
    ///
    /// # Errors
    ///
    /// This function will only return an error when `src` only contains
    /// whitespace, or is empty.
    pub fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{branch::alt, combinator::map};

        let (new_src, tok) = alt((
            map(Operator::parse, Token::Operator),
            map(Delimiter::parse, Token::Delimiter),
            map(Keyword::parse, Token::Keyword),
            map(Literal::parse, Token::Literal),
            map(Ident::parse, Token::Ident),
        ))(src)?;

        if new_src.chars().next().is_some_and(char::is_alphabetic)
            && matches!(tok, Token::Keyword(_))
        {
            // This is _not_ a keyword, but an identifier.
            // Re-parse it as such.
            return map(Ident::parse, Token::Ident)(src);
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
        ::paste::paste! {
            $(#[$outer])*
            $vis enum $name {
                $($variant,)+
            }

            impl $name {
                $(
                    pub const [<$variant:upper>]: &'static str =
                        $symbol;
                )+

                #[doc = concat!(
                    "Parse a [`",
                    stringify!([< $name >]),
                    "`] from the beginning of a [`Source`] slice.",
                    "\n\n",
                    "# Errors",
                    "\n\n",
                    "This function returns an [`Err`] if a [`",
                    stringify!([< $name >]),
                    "`] cannot be parsed.",
                )]
                fn parse(src: $crate::Source) -> $crate::Result<Self> {
                    ::nom::branch::alt((
                        $(::nom::combinator::map(
                            ::nom::bytes::complete::tag($symbol),
                            |_| $name::$variant
                        ),)+
                    ))(src)
                }
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
    Number(Source<'s>),
    String(Source<'s>),
}

impl<'s> Literal<'s> {
    fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_until, take_while1},
            combinator::{map, opt, recognize},
            sequence::tuple,
        };

        alt((
            map(
                recognize(tuple((
                    opt(tag("-")),
                    take_while1(|ch: char| ch.is_ascii_digit()),
                ))),
                Self::Number,
            ),
            map(
                recognize(tuple((tag("\""), take_until("\""), tag("\"")))),
                Self::String,
            ),
        ))(src)
    }
}

/// A Monkey identifier.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Ident<'s>(pub Source<'s>);

impl<'s> Ident<'s> {
    /// Returns the inner identifier.
    #[must_use]
    pub fn get(&self) -> Source {
        self.0
    }

    /// Parses an [`Ident`] from the beginning of a [`Source`] slice.
    ///
    /// # Errors
    ///
    /// Identifiers may only contain `_` and _alphanumeric_ characters,
    /// and must start with either `_` or _alphabetic_ characters.
    ///
    /// This function returns an [`Err`] if the above condition is not met.
    pub fn parse(src: Source<'s>) -> Result<Self> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take, take_while},
            combinator::{map, recognize, verify},
            sequence::tuple,
        };

        // Identifiers may only start with `_` or
        // alphabetic characters.

        map(
            recognize(tuple((
                alt((
                    tag("_"),
                    verify(take(1usize), |s: &str| {
                        s.chars().next().is_some_and(char::is_alphabetic)
                    }),
                )),
                take_while(|ch: char| ch == '_' || ch.is_ascii_alphanumeric()),
            ))),
            Self,
        )(src)
    }
}

#[cfg(test)]
mod tests {
    use super::{Delimiter, Ident, Keyword, Literal, Operator, Token};

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
            Token::Literal(Literal::Number("123"))
        );
        assert_eq!(
            Token::parse("_ident").unwrap().1,
            Token::Ident(Ident("_ident")),
        )
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
            Literal::Number("816723")
        );
        assert_eq!(
            Literal::parse(r#""hello, world!""#).unwrap().1,
            Literal::String(r#""hello, world!""#)
        )
    }

    #[test]
    fn ident() {
        assert_eq!(
            Ident::parse("__some_ident123").unwrap().1,
            Ident("__some_ident123"),
        );
        assert!(Ident::parse("1_invalid_ident").is_err());
    }
}
