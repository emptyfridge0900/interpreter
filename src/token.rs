use std::{collections::HashMap, fmt};

use crate::ast::{Expression, Node, Statement};

pub type TokenType = String;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String), // add, foobar, x, y, ...
    INT(i64),   // 1343456

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    //Comparison operators
    EQ,
    NOT_EQ,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}
impl Token{
    pub fn token_type(&self)->String{
        match self{
            Token::IDENT(i)=>"ident".to_owned(),
            Token::INT(i)=>"int".to_owned(),
            _=>self.to_string().to_lowercase()
        }
    }
    pub fn token_value(&self)->String{
        match self{
            Token::IDENT(i)=>i.to_owned(),
            Token::INT(i)=>i.to_string(),
            _=>self.to_string().to_lowercase()
        }
    }
}
impl fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self{
            Token::IDENT(x)=>{
                write!(f, "{}", x)
            },
            Token::INT(x)=>{
                write!(f, "{}", x)
            },
            _=> write!(f, "{:?}", self)
        }
    }
}