use std::io::{stdin, stdout, Write};

use crate::{ast::{Node, Program}, lexer::{self, Lexer}, parser::Parser, token::{self, Token}};

pub fn start(){
    loop{
        let mut s=String::new();
        print!(">>");
        let _=stdout().flush();
        stdin().read_line(&mut s).expect("Did not enter a correct string");
        let mut l= Lexer::new(&s);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        
        if p.errors().len() !=0{
            print_parser_error(p.errors);
            continue;
        }
        println!("{}",program.string());

    }
}
fn print_parser_error(errors:Vec<String>){
    println!("{}",MONKEY_FACE);
    println!("Woops! We ran into some monkey business here!");
    println!("parser errors:");

    for msg in errors{
        println!("{}",msg);
    }
}

const MONKEY_FACE: &str = r#"
          __,__
 .--.  .-"     "-.  .--.
/ .. \/ .-. .-.   \/ .. \
| | '| /   Y   \  |'  | |
| \ \ \  0 | 0 / /    / |
\ '- ,\.-"""""""-./, -' /
''-' /  _ ^ ^ _   \ '-''
    |  \._   _./   |
    \   \ '~' /   /
    '._  '-=-' _.'
       '-----'"#;