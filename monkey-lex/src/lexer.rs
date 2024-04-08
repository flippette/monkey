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
    use crate::token::{Delimiter, Keyword, Literal, Operator, Token};

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
                Token::Delimiter(Delimiter::Semi),
                // let ten = 10;
                Token::Keyword(Keyword::Let),
                Token::Ident("ten"),
                Token::Operator(Operator::Eq),
                Token::Literal(Literal::Integer(10)),
                Token::Delimiter(Delimiter::Semi),
                // let add = fn(x, y) {
                Token::Keyword(Keyword::Let),
                Token::Ident("add"),
                Token::Operator(Operator::Eq),
                Token::Keyword(Keyword::Fn),
                Token::Operator(Operator::LParen),
                Token::Ident("x"),
                Token::Delimiter(Delimiter::Comma),
                Token::Ident("y"),
                Token::Operator(Operator::RParen),
                Token::Operator(Operator::LBrace),
                //     x + y;
                Token::Ident("x"),
                Token::Operator(Operator::Plus),
                Token::Ident("y"),
                Token::Delimiter(Delimiter::Semi),
                // };
                Token::Operator(Operator::RBrace),
                Token::Delimiter(Delimiter::Semi),
                // let result = add(five, ten);
                Token::Keyword(Keyword::Let),
                Token::Ident("result"),
                Token::Operator(Operator::Eq),
                Token::Ident("add"),
                Token::Operator(Operator::LParen),
                Token::Ident("five"),
                Token::Delimiter(Delimiter::Comma),
                Token::Ident("ten"),
                Token::Operator(Operator::RParen),
                Token::Delimiter(Delimiter::Semi),
            ],
        );
    }

    #[test]
    fn ext() {
        let src = "!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
10 >= 9;
9 <= 10;
";

        assert_tokens!(
            src,
            [
                // !-/*5;
                Token::Operator(Operator::Bang),
                Token::Operator(Operator::Minus),
                Token::Operator(Operator::Slash),
                Token::Operator(Operator::Star),
                Token::Literal(Literal::Integer(5)),
                Token::Delimiter(Delimiter::Semi),
                // 5 < 10 > 5;
                Token::Literal(Literal::Integer(5)),
                Token::Operator(Operator::Lt),
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::Gt),
                Token::Literal(Literal::Integer(5)),
                Token::Delimiter(Delimiter::Semi),
                // if (5 < 10) {
                Token::Keyword(Keyword::If),
                Token::Operator(Operator::LParen),
                Token::Literal(Literal::Integer(5)),
                Token::Operator(Operator::Lt),
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::RParen),
                Token::Operator(Operator::LBrace),
                //     return true;
                Token::Keyword(Keyword::Return),
                Token::Keyword(Keyword::True),
                Token::Delimiter(Delimiter::Semi),
                // } else {
                Token::Operator(Operator::RBrace),
                Token::Keyword(Keyword::Else),
                Token::Operator(Operator::LBrace),
                //     return false;
                Token::Keyword(Keyword::Return),
                Token::Keyword(Keyword::False),
                Token::Delimiter(Delimiter::Semi),
                // }
                Token::Operator(Operator::RBrace),
                // 10 == 10;
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::EqEq),
                Token::Literal(Literal::Integer(10)),
                Token::Delimiter(Delimiter::Semi),
                // 10 != 9;
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::Ne),
                Token::Literal(Literal::Integer(9)),
                Token::Delimiter(Delimiter::Semi),
                // 10 >= 9;
                Token::Literal(Literal::Integer(10)),
                Token::Operator(Operator::Ge),
                Token::Literal(Literal::Integer(9)),
                Token::Delimiter(Delimiter::Semi),
                // 9 <= 10;
                Token::Literal(Literal::Integer(9)),
                Token::Operator(Operator::Le),
                Token::Literal(Literal::Integer(10)),
                Token::Delimiter(Delimiter::Semi),
            ],
        );
    }

    #[test]
    fn operators() {
        let src = "==!=<=>==+-*/!<>(){},;";
        assert_tokens!(
            src,
            [
                Token::Operator(Operator::EqEq),
                Token::Operator(Operator::Ne),
                Token::Operator(Operator::Le),
                Token::Operator(Operator::Ge),
                Token::Operator(Operator::Eq),
                Token::Operator(Operator::Plus),
                Token::Operator(Operator::Minus),
                Token::Operator(Operator::Star),
                Token::Operator(Operator::Slash),
                Token::Operator(Operator::Bang),
                Token::Operator(Operator::Lt),
                Token::Operator(Operator::Gt),
                Token::Operator(Operator::LParen),
                Token::Operator(Operator::RParen),
                Token::Operator(Operator::LBrace),
                Token::Operator(Operator::RBrace),
                Token::Delimiter(Delimiter::Comma),
                Token::Delimiter(Delimiter::Semi),
            ]
        );
    }

    #[test]
    fn keywords() {
        let src = "fn let true false if else return";
        assert_tokens!(
            src,
            [
                Token::Keyword(Keyword::Fn),
                Token::Keyword(Keyword::Let),
                Token::Keyword(Keyword::True),
                Token::Keyword(Keyword::False),
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Else),
                Token::Keyword(Keyword::Return),
            ],
        );
    }

    #[test]
    fn keywords_invalid() {
        let src = "fnlettruefalse";
        assert_tokens!(src, [Token::Ident("fnlettruefalse")]);
    }
}
