use std::{collections::HashMap, str::from_utf8};

use crate::token::{self, Token};

pub struct Lexer {
    pub input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.as_bytes().to_owned(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    pub fn peek_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }
    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn read_number(&mut self) -> i64 {
        let position = self.position;
        while Self::is_digit(self.ch) {
            self.read_char()
        }
        from_utf8(&self.input[position..self.position])
            .unwrap()
            .to_string()
            .parse::<i64>()
            .unwrap()
    }

    pub fn read_string(&mut self)->String{
        let position = self.position+1;
        loop{
            self.read_char();
            if self.ch == b'"' || self.ch == 0{
                break
            }
        }
        from_utf8(&self.input[position..self.position])
            .unwrap()
            .to_string()
    }

    pub fn next_token(&mut self) -> Token {
        let tok: token::Token;
        self.skip_whitespace();
        match self.ch {
            b'"'=>{
                tok=Token::STRING(self.read_string())
            },
            b'=' => {
                tok = {
                    if self.peek_char() == b'=' {
                        let ch = self.ch;
                        self.read_char();
                        from_utf8(&[ch, self.ch]).unwrap().to_string();
                        Token::EQ
                    } else {
                        Token::ASSIGN
                    }
                }
            }
            b'+' => tok = Token::PLUS,
            b'-' => tok = Token::MINUS,
            b'!' => {
                tok = {
                    if self.peek_char() == b'=' {
                        let ch = self.ch;
                        self.read_char();
                        from_utf8(&[ch, self.ch]).unwrap().to_string();
                        Token::NOT_EQ
                    } else {
                        Token::BANG
                    }
                }
            }
            b'*' => tok = Token::ASTERISK,
            b'/' => tok = Token::SLASH,
            b'<' => tok = Token::LT,
            b'>' => tok = Token::GT,
            b';' => tok = Token::SEMICOLON,
            b',' => tok = Token::COMMA,
            b'(' => tok = Token::LPAREN,
            b')' => tok = Token::RPAREN,
            b'{' => tok = Token::LBRACE,
            b'}' => tok = Token::RBRACE,
            0 => tok = Token::EOF,
            _ => {
                if Self::is_letter(self.ch) {
                    let lit = self.read_identifier();
                    tok = Self::lookup_ident(&lit);
                    return tok;
                } else if Self::is_digit(self.ch) {
                    let num = self.read_number();
                    tok = Token::INT(num);
                    return tok;
                } else {
                    println!("{}", self.ch);
                    tok = Token::ILLEGAL
                }
            }
        }

        self.read_char();
        tok
    }
    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        from_utf8(&self.input[position..self.position])
            .unwrap()
            .to_string()
    }

    pub fn is_letter(ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }

    pub fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    pub fn lookup_ident(ident: &str) -> token::Token {
        let map: HashMap<&str, Token> = HashMap::from([
            ("fn", Token::FUNCTION),
            ("let", Token::LET),
            ("true", Token::TRUE),
            ("false", Token::FALSE),
            ("if", Token::IF),
            ("else", Token::ELSE),
            ("return", Token::RETURN),
        ]);
        map.get(ident)
            .unwrap_or(&Token::IDENT(ident.to_string()))
            .clone()
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;


    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
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
"foobar"
"foo bar"
"#;

        let tests: Vec<Token> = vec![
            Token::LET,
            Token::IDENT("five".to_owned()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_owned()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_owned()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_owned()),
            Token::COMMA,
            Token::IDENT("y".to_owned()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_owned()),
            Token::PLUS,
            Token::IDENT("y".to_owned()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_owned()),
            Token::ASSIGN,
            Token::IDENT("add".to_owned()),
            Token::LPAREN,
            Token::IDENT("five".to_owned()),
            Token::COMMA,
            Token::IDENT("ten".to_owned()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOT_EQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::STRING("foobar".to_string()),
            Token::STRING("foo bar".to_string()),
            Token::EOF,
        ];
        let mut l = Lexer::new(input);
        for (i, tt) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            if tok != tt {
                panic!(
                    "tests[{}] - tokentype wrong. expected={}, got={}",
                    i, tt, tok
                );
            }
        }
    }


}
