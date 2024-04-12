use std::fmt::{self};

use crate::parser::Precedences;

pub type TokenType = String;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String), // add, foobar, x, y, ...
    INT(i64),   // 1343456
    STRING(String),

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
    LBRACKET,
    RBRACKET,

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
            Token::STRING(i)=>"string".to_owned(),
            _=>self.to_string().to_lowercase()
        }
    }
    pub fn token_value(&self)->String{
        match self{
            Token::IDENT(i)=>i.to_owned(),
            Token::INT(i)=>i.to_string(),
            Token::STRING(i)=>i.to_string(),
            _=>self.to_string().to_lowercase()
        }
    }
    pub fn precedence(&self)->Precedences{
        match self{
            Token::EQ=>Precedences::EQUALS,
            Token::NOT_EQ=>Precedences::EQUALS,
            Token::LT=>Precedences::LESSGREATER,
            Token::GT=>Precedences::LESSGREATER,
            Token::PLUS=>Precedences::SUM,
            Token::MINUS=>Precedences::SUM,
            Token::SLASH=>Precedences::PRODUCT,
            Token::ASTERISK=>Precedences::PRODUCT,
            Token::LPAREN=>Precedences::CALL,
            Token::LBRACKET=>Precedences::INDEX,
            _=>Precedences::LOWEST
        }
    }
}
impl fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self{
            Token::IDENT(x)=>write!(f, "{x}") ,
            Token::INT(x)=>write!(f, "{x}"),
            Token::STRING(x)=>write!(f,"{x}"),
            Token::ILLEGAL=>write!(f,"ILLEGAL"),
            Token::EOF=>write!(f,"EOF"),
            Token::ASSIGN=>write!(f,"="),
            Token::PLUS=>write!(f,"+"),
            Token::MINUS=>write!(f,"-"),
            Token::BANG=>write!(f,"!"),
            Token::ASTERISK=>write!(f,"*"),
            Token::SLASH=>write!(f,"/"),
            Token::LT=>write!(f,"<"),
            Token::GT=>write!(f,">"),
            Token::EQ=>write!(f,"=="),
            Token::NOT_EQ=>write!(f,"!="),
            Token::COMMA=>write!(f,","),
            Token::SEMICOLON=>write!(f,";"),
            Token::LPAREN=>write!(f,"("),
            Token::RPAREN=>write!(f,")"),
            Token::LBRACE=>write!(f,"{{"),
            Token::RBRACE=>write!(f,"}}"),
            Token::LBRACKET=>write!(f,"["),
            Token::RBRACKET=>write!(f,"]"),
            Token::FUNCTION=>write!(f,"FUNCTION"),
            Token::LET=>write!(f,"LET"),
            Token::TRUE=>write!(f,"TRUE"),
            Token::FALSE=>write!(f,"FALSE"),
            Token::IF=>write!(f,"IF"),
            Token::ELSE=>write!(f,"ELSE"),
            Token::RETURN=>write!(f,"RETURN"),
        }
    }
}