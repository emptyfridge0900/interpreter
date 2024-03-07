use std::{any::Any, cell::RefCell};

use crate::token::{self, Token};


pub trait Node {
    fn token_literal(&self)->String;
    fn as_any(&self)->&dyn Any;
}

pub trait Statement:Node{
    fn statement_node(&self);
}

pub trait Expression:Node{
    fn expression_node(&self);
}




pub struct Program{
    pub statements:RefCell<Vec<Box<dyn Statement>>>
}
impl Program{
    pub fn new()->Program{
        Program{
            statements:RefCell::new(vec![])
        }
    }
}
impl Node for Program{
    fn token_literal(&self)->String {
        if self.statements.borrow().len()>0{
            self.statements.borrow()[0].as_ref().token_literal().clone()
        }else{
            "".to_string()
        }
    }
    
    fn as_any(&self)->&dyn Any {
        self
    }
}






pub struct LetStatement{
    pub token: Token,
    pub name: Identifier,
    pub value:Option<Box<dyn Expression>>
}
impl LetStatement{
    pub fn new(token:Token,name:Identifier)->LetStatement{
        LetStatement{
            token,
            name,
            value:None
        }
    }
}
impl Node for LetStatement{
    fn token_literal(&self)->String {
        self.token.literal.clone()
    }
    
    fn as_any(&self)->&dyn Any {
        self
    }
}
impl Statement for LetStatement{
    fn statement_node(&self) {
    }
}






pub struct Identifier{
    pub token:Token,
    pub value:String
}
impl Identifier{
    pub fn new(token:Token, value:String)->Identifier{
        Identifier{
            token,
            value
        }
    }
}
impl Node for Identifier{
    fn token_literal(&self)->String {
        self.token.literal.clone()
    }
    
    fn as_any(&self)->&dyn Any {
        self
    }
}
impl Statement for Identifier{
    fn statement_node(&self) {
    }
}






pub struct ReturnStatement{
    token: token::Token,
    return_value: Option<Box<dyn Expression>>
}
impl ReturnStatement{
    pub fn new(token:Token)->ReturnStatement{
        ReturnStatement{
            token,
            return_value:None
        }
    }
}
impl Node for ReturnStatement{
    fn token_literal(&self)->String {
        self.token.literal.clone()
    }

    fn as_any(&self)->&dyn Any {
        self
    }
}
impl Statement for ReturnStatement{
    fn statement_node(&self) {
    }
}