use std::{collections::HashMap, str::from_utf8};

use crate::token::{self, Token, TokenType};


pub struct Lexer<'a>{
    pub input:&'a [u8],
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

    pub fn read_number(&mut self)->String{
        let position=self.position;
        while Self::is_digit(self.ch){
            self.read_char()
        }
        from_utf8(&self.input[position..self.position]).unwrap().to_string()
    }

    pub fn next_token(&mut self)->Token{
        let mut tok : token::Token;
        self.skip_whitespace();
        match self.ch{
            b'='=> tok = Token::new(token::ASSIGN.to_owned(),"=".to_owned()),
            b';'=> tok = Token::new(token::SEMICOLON.to_owned(),";".to_owned()),
            b'('=> tok = Token::new(token::LPAREN.to_owned(),"(".to_owned()),
            b')'=> tok = Token::new(token::RPAREN.to_owned(),")".to_owned()),
            b','=> tok = Token::new(token::COMMA.to_owned(),",".to_owned()),
            b'+'=> tok = Token::new(token::PLUS.to_owned(),"+".to_owned()),
            b'{'=> tok = Token::new(token::LBRACE.to_owned(),"{".to_owned()),
            b'}'=> tok = Token::new(token::RBRACE.to_owned(),"}".to_owned()),
            0=> {
                tok=Token::new(token::EOF.to_owned(),"".to_owned())
            },
            _=>{
                if Self::is_letter(self.ch){
                    let lit=self.read_identifier();
                    tok= Token::new(Self::lookup_ident(&lit),lit.clone());
                    return tok;
                }else if Self::is_digit(self.ch){
                    let num=self.read_number();
                    tok= Token::new(token::INT.to_owned(),num.clone());
                    return tok;
                }else{
                    println!("{}",self.ch);
                    tok= Token::new(token::ILLEGAL.to_owned(),self.ch.to_string())
                }
            }
        }

        self.read_char();
        tok
    }
    pub fn read_identifier(&mut self)->String{
        let position = self.position;
        while Self::is_letter(self.ch){
           self.read_char(); 
        }
        from_utf8(&self.input[position..self.position]).unwrap().to_string()
    }

    pub fn is_letter(ch:u8)->bool{
        b'a'<=ch && ch <= b'z' ||b'A'<=ch &&ch <= b'Z' || ch == b'_'
    }

    pub fn is_digit(ch:u8)->bool{
        b'0' <= ch && ch <= b'9'
    }
    
    pub fn lookup_ident(ident:&str)->token::TokenType{
        let map: HashMap<String, TokenType> =HashMap::from([
            ("fn".to_owned(), "FUNCTION".to_owned()),
            ("let".to_owned(), "LET".to_owned()),
        ]);
        map.get(ident).unwrap_or(&"IDENT".to_string()).to_owned()
    }

    pub fn skip_whitespace(&mut self){
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
}