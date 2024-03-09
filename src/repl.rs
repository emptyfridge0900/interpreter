use std::io::{stdin, stdout, Write};

use crate::{lexer::{self, Lexer}, token::{self, Token}};

pub fn start(){
    let mut s=String::new();
    loop{
        print!(">>");
        let _=stdout().flush();
        stdin().read_line(&mut s).expect("Did not enter a correct string");
        let mut l= Lexer::new(&s);
        let mut tok=l.next_token();
        while tok!=Token::EOF{
            println!("{:?}",tok);
            tok = l.next_token();
        }
    }
}