use crate::{Result, Source};

/// A Monkey source code token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Token<'s> {
    /// Identifier.
    Ident(Source<'s>),
    /// Operator.
    Operator(Operator),
    /// Keyword.
    Keyword(Keyword),
    /// Literal.
    Literal(Literal),
}

impl<'s> Token<'s> {
    pub fn parse(src: Source) -> Result<Self> {
        use nom::{branch::alt, combinator::map};

        alt((
            map(Operator::parse, Token::Operator),
            map(Keyword::parse, Token::Keyword),
            map(Literal::parse, Token::Literal),
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
            pub fn parse(src: $crate::Source) -> $crate::Result<Self> {
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
    pub fn parse(src: Source) -> Result<Self> {
        use nom::{branch::alt, character::complete::u64, combinator::map};

        alt((
            map(u64, Literal::Integer),
            // other
        ))(src)
    }
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
}
