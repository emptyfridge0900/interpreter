
mod token;
mod lexer;
use token::{Token,TokenType};

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, token, Expected};

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn test_next_token(){
        let input = "=+(){},;";

        let tests:[Expected;9]=[
            Expected::new(token::ASSIGN,"="),
            Expected::new(token::PLUS,"+"),
            Expected::new(token::LPAREN,"("),
            Expected::new(token::RPAREN,")"),
            Expected::new(token::LBRACE,"{"),
            Expected::new(token::RBRACE,"}"),
            Expected::new(token::COMMA,","),
            Expected::new(token::SEMICOLON,";"),
            Expected::new(token::EOF,""),
        ];
        let mut l=Lexer::new(input);

        for (i,tt) in tests.into_iter().enumerate(){
            let tok=l.next_token();
            if tok.token_type != tt.expectedType{

            }

            if tok.literal != tt.expectedLiteral{

            }
        }
    }
}

struct Expected{
    expectedType:TokenType,
    expectedLiteral:&'static str
}
impl Expected{
    fn new(t:TokenType,l:&'static str)->Self{
        Expected{
            expectedType:t,
            expectedLiteral:l
        }
    }
}