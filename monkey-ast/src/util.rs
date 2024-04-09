use monkey_lex::{lexer::Lexer, token::Token, Source};

/// A [`Lexer`] that can go backwards (similar to what `ReverseIterator` would
/// be if it existed, and **not** [`DoubleEndedIterator`]).
#[derive(Clone, Debug)]
pub struct SeekLexer<'s> {
    inner: Lexer<'s>,
    cache: Vec<Token<'s>>,
    idx: usize,
}

/// A wrapper for [`SeekLexer::last`] that implements [`Iterator`].
#[derive(Debug)]
pub struct Reverse<'l, 's> {
    inner: &'l mut SeekLexer<'s>,
}

impl<'s> SeekLexer<'s> {
    /// Creates a [`SeekLexer`] from a [`Lexer`].
    #[must_use]
    pub fn from_lexer(lexer: Lexer<'s>) -> Self {
        Self {
            inner: lexer,
            cache: vec![],
            idx: 0,
        }
    }

    /// Creates a [`SeekLexer`] from a [`Source`] slice.
    #[must_use]
    pub fn from_src(src: Source<'s>) -> Self {
        Self::from_lexer(Lexer::new(src))
    }

    /// Gets the last token in iteration, opposite of [`Iterator::next`].
    pub fn last(&mut self) -> Option<Token<'s>> {
        if self.idx == 0 {
            None
        } else {
            self.idx -= 1;
            self.cache.get(self.idx).copied()
        }
    }

    /// Create a [`Reverse`] borrowing from this [`SeekLexer`].
    pub fn reverse(&mut self) -> Reverse<'_, 's> {
        Reverse { inner: self }
    }
}

impl<'s> Iterator for SeekLexer<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.idx {
            idx if idx >= self.cache.len() => {
                // end of cache, parse more
                let next_tok = self.inner.next()?;
                self.cache.push(next_tok);
                self.idx += 1;
                Some(next_tok)
            }
            idx => {
                self.idx += 1;
                self.cache.get(idx).copied()
            }
        }
    }
}

impl<'l, 's> Iterator for Reverse<'l, 's> {
    type Item = <SeekLexer<'s> as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.last()
    }
}

#[cfg(test)]
mod tests {
    use monkey_lex::token::{
        Delimiter, Ident, Keyword, Literal, Operator, Token,
    };

    use super::SeekLexer;

    macro_rules! assert_iter {
        ($lexer:expr $(,> $tok:expr)*) => {
            $(assert_eq!(dbg!($lexer.next()), Some($tok));)*
        };
        ($lexer:expr $(,> $tok:expr)* ;) => {
            assert_iter! { $lexer $(,> $tok)+ };
            assert!(dbg!($lexer.next()).is_none());
        };
    }

    #[test]
    fn iter_full() {
        let src = "fn 123 _ident == != ;";

        let mut lexer = SeekLexer::from_src(src);
        dbg!(&lexer);

        assert_iter! {
            lexer
                ,> Token::Keyword(Keyword::Fn)
                ,> Token::Literal(Literal::Number("123"))
                ,> Token::Ident(Ident("_ident"))
                ,> Token::Operator(Operator::EqEq)
                ,> Token::Operator(Operator::Ne)
                ,> Token::Delimiter(Delimiter::Semi)
                ;
        };

        assert_iter! {
            lexer.reverse()
                ,> Token::Delimiter(Delimiter::Semi)
                ,> Token::Operator(Operator::Ne)
                ,> Token::Operator(Operator::EqEq)
                ,> Token::Ident(Ident("_ident"))
                ,> Token::Literal(Literal::Number("123"))
                ,> Token::Keyword(Keyword::Fn)
                ;
        };
    }

    #[test]
    fn iter_halfway() {
        let src = "_ident \"literal\" !=";

        let mut lexer = SeekLexer::from_src(src);

        assert_iter! {
            lexer
                ,> Token::Ident(Ident("_ident"))
                ,> Token::Literal(Literal::String("\"literal\""))
                ,> Token::Operator(Operator::Ne)
                ;
        };

        assert_iter! {
            lexer.reverse()
                ,> Token::Operator(Operator::Ne)
                ,> Token::Literal(Literal::String("\"literal\""))
        };

        assert_iter! {
            lexer
                ,> Token::Literal(Literal::String("\"literal\""))
                ,> Token::Operator(Operator::Ne)
        };
    }
}
