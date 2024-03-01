use crate::token::{self, Token};


pub struct Lexer<'a>{
    input:&'a [u8],
    position:usize,
    read_position:usize,
    ch:u8
}

impl<'a> Lexer<'a>{
    pub fn new(input:&'a str)->Lexer<'a>{
        let mut l =Lexer{
            input:input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }
    pub fn read_char(&mut self){
        if self.read_position>=self.input.len(){
            self.ch=0;
        }else{
            self.ch=self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position+=1;
    }
    pub fn next_token(&mut self)->Token{
        let tok : token::Token;
        match self.ch{
            b'='=> tok = Token::new(token::ASSIGN,"="),
            b';'=> tok = Token::new(token::SEMICOLON,";"),
            b'('=> tok = Token::new(token::LPAREN,"("),
            b')'=> tok = Token::new(token::RPAREN,")"),
            b','=> tok = Token::new(token::COMMA,","),
            b'+'=> tok = Token::new(token::PLUS,"+"),
            b'{'=> tok = Token::new(token::LBRACE,"{"),
            b'}'=> tok = Token::new(token::RBRACE,"}"),
            0=> {
                tok=Token::new(token::EOF,"")
            },
            _=>unreachable!()
        }

        self.read_char();
        tok
    }
}