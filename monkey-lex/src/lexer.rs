use crate::{token::Token, Source};

/// A Monkey source code lexer.
#[derive(Clone, Debug)]
pub struct Lexer<'s> {
    src: Source<'s>,
}

impl<'s> Lexer<'s> {
    /// Create a new [`Lexer`] from a [`Source`] slice.
    pub fn new(src: Source<'s>) -> Self {
        Self { src }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Token<'s>> {
        let (src, tok) = Token::parse(self.src.trim_start()).ok()?;
        self.src = src;
        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{Keyword, Literal, Operator, Token};

    macro_rules! assert_tokens {
        ($src:expr, [$($tok:expr),+ $(,)?] $(,)?) => {
            let mut lex = $crate::lexer::Lexer::new($src);
            $(
                assert_eq!(lex.next(), Some($tok));
                dbg!(&lex.src.lines().next());
            )+
        };
    }

    #[test]
    fn add() {
        let src = "let five = 5;
let ten = 10;

let add = fn(x, y) {
   x + y;
};

let result = add(five, ten);";

        assert_tokens!(
            src,
            [
                // let five = 5;
                Token::Keyword(Keyword::Let),
                Token::Ident("five"),
                Token::Operator(Operator::Eq),
                Token::Literal(Literal::Integer(5)),
                Token::Operator(Operator::Semi),
                // let ten = 10;
                Token::Keyword(Keyword::Let),
                Token::Ident("ten"),
                Token::Operator(Operator::Eq),
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::Semi),
                // let add = fn(x, y) {
                Token::Keyword(Keyword::Let),
                Token::Ident("add"),
                Token::Operator(Operator::Eq),
                Token::Keyword(Keyword::Fn),
                Token::Operator(Operator::LParen),
                Token::Ident("x"),
                Token::Operator(Operator::Comma),
                Token::Ident("y"),
                Token::Operator(Operator::RParen),
                Token::Operator(Operator::LBrace),
                //     x + y;
                Token::Ident("x"),
                Token::Operator(Operator::Plus),
                Token::Ident("y"),
                Token::Operator(Operator::Semi),
                // };
                Token::Operator(Operator::RBrace),
                Token::Operator(Operator::Semi),
                // let result = add(five, ten);
                Token::Keyword(Keyword::Let),
                Token::Ident("result"),
                Token::Operator(Operator::Eq),
                Token::Ident("add"),
                Token::Operator(Operator::LParen),
                Token::Ident("five"),
                Token::Operator(Operator::Comma),
                Token::Ident("ten"),
                Token::Operator(Operator::RParen),
                Token::Operator(Operator::Semi),
            ],
        );
    }
}
