
use std::collections::HashMap;

pub type TokenType =String;

pub struct Token{
    pub token_type: TokenType,
    pub literal:String
}
impl Token{
    pub fn new(t:TokenType,l:String)->Token{
        Token{
            token_type:t,
            literal:l
        }
    }
}

pub const ILLEGAL: &str="ILLEGAL";
pub const EOF: &str = "EOF";
// Identifiers + literals
pub const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
pub const INT: &str = "INT"; // 1343456
// Operators
pub const ASSIGN: &'static str = "=";
pub const PLUS: &str = "+";
// Delimiters
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
// Keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";

