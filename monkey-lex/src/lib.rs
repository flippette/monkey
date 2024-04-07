//! A lexer for the Monkey programming language.

#![feature(decl_macro)]

pub type Source<'a> = &'a str;
pub type Result<'a, T> = nom::IResult<Source<'a>, T>;

/// Tokens in Monkey.
pub mod token;

/// Lexer implementation.
pub mod lexer;
