
pub mod token;
pub mod lexer;
pub mod repl;
use token::{Token,TokenType};

#[cfg(test)]
mod tests {
    use core::panic;
    use std::str::from_utf8;

    use crate::{lexer::Lexer, token, Expected};

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn test_next_token(){
        
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";

        let tests:[Expected;74]=[
            Expected::new(token::LET.to_owned(),"let"),
            Expected::new(token::IDENT.to_owned(),"five"),
            Expected::new(token::ASSIGN.to_owned(),"="),
            Expected::new(token::INT.to_owned(),"5"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::LET.to_owned(),"let"),
            Expected::new(token::IDENT.to_owned(),"ten"),
            Expected::new(token::ASSIGN.to_owned(),"="),
            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::LET.to_owned(),"let"),
            Expected::new(token::IDENT.to_owned(),"add"),
            Expected::new(token::ASSIGN.to_owned(),"="),
            Expected::new(token::FUNCTION.to_owned(),"fn"),
            Expected::new(token::LPAREN.to_owned(),"("),
            Expected::new(token::IDENT.to_owned(),"x"),
            Expected::new(token::COMMA.to_owned(),","),
            Expected::new(token::IDENT.to_owned(),"y"),
            Expected::new(token::RPAREN.to_owned(),")"),
            Expected::new(token::LBRACE.to_owned(),"{"),
            Expected::new(token::IDENT.to_owned(),"x"),
            Expected::new(token::PLUS.to_owned(),"+"),
            Expected::new(token::IDENT.to_owned(),"y"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::RBRACE.to_owned(),"}"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::LET.to_owned(),"let"),
            Expected::new(token::IDENT.to_owned(),"result"),
            Expected::new(token::ASSIGN.to_owned(),"="),
            Expected::new(token::IDENT.to_owned(),"add"),
            Expected::new(token::LPAREN.to_owned(),"("),
            Expected::new(token::IDENT.to_owned(),"five"),
            Expected::new(token::COMMA.to_owned(),","),
            Expected::new(token::IDENT.to_owned(),"ten"),
            Expected::new(token::RPAREN.to_owned(),")"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::BANG.to_owned(),"!"),
            Expected::new(token::MINUS.to_owned(),"-"),
            Expected::new(token::SLASH.to_owned(),"/"),
            Expected::new(token::ASTERISK.to_owned(),"*"),
            Expected::new(token::INT.to_owned(),"5"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::INT.to_owned(),"5"),
            Expected::new(token::LT.to_owned(),"<"),
            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::GT.to_owned(),">"),
            Expected::new(token::INT.to_owned(),"5"),
            Expected::new(token::SEMICOLON.to_owned(),";"),

            Expected::new(token::IF.to_owned(),"if"),
            Expected::new(token::LPAREN.to_owned(),"("),
            Expected::new(token::INT.to_owned(),"5"),
            Expected::new(token::LT.to_owned(),"<"),
            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::RPAREN.to_owned(),")"),
            Expected::new(token::LBRACE.to_owned(),"{"),
            Expected::new(token::RETURN.to_owned(),"return"),
            Expected::new(token::TRUE.to_owned(),"true"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::RBRACE.to_owned(),"}"),
            Expected::new(token::ELSE.to_owned(),"else"),
            Expected::new(token::LBRACE.to_owned(),"{"),
            Expected::new(token::RETURN.to_owned(),"return"),
            Expected::new(token::FALSE.to_owned(),"false"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::RBRACE.to_owned(),"}"),

            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::EQ.to_owned(),"=="),
            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::SEMICOLON.to_owned(),";"),
            Expected::new(token::INT.to_owned(),"10"),
            Expected::new(token::NOT_EQ.to_owned(),"!="),
            Expected::new(token::INT.to_owned(),"9"),
            Expected::new(token::SEMICOLON.to_owned(),";"),

            Expected::new(token::EOF.to_owned(),""),
        ];
        let mut l=Lexer::new(input);
        for (i,tt) in tests.into_iter().enumerate(){
            let tok=l.next_token();
            if tok.token_type != tt.expectedType{
                panic!("tests[{}] - tokentype wrong. expected={}, got={}",i, tt.expectedType, tok.token_type);
            }

            if tok.literal != tt.expectedLiteral{
                panic!("tests[{}] - literal wrong. expected={}, got={}",i, tt.expectedLiteral, tok.literal);
            }
        }
    }
}

struct Expected<'a>{
    expectedType:TokenType,
    expectedLiteral:&'a str
}
impl<'a> Expected<'a>{
    fn new(t:TokenType,l:&'a str)->Self{
        Expected{
            expectedType:t,
            expectedLiteral:l
        }
    }
}