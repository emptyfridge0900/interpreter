use std::rc::Rc;

use crate::{ast::{Identifier, Statement}, environment::Environment};


#[derive(PartialEq,Eq,Clone,Debug)]
pub enum Object{
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function{parameters:Vec<Identifier>,body:Statement,env:Environment},
    String(String),
    Builtin(fn(args:Vec<Object>) -> Object),
    Error(String),
    Unknown
}

impl Object{
    pub fn get_type(&self)->&str{
        match self{
            Object::Integer(_)=>"INTEGER",
            Object::Boolean(_)=>"BOOLEAN",
            Object::Null=>"NULL",
            Object::Return(_)=>"RETURN_VALUE",
            Object::Function{..}=>"FUNCTION",
            Object::String(_)=>"STRING",
            Object::Builtin(_)=>"BUILTIN",
            Object::Error(_)=>"ERROR",
            Object::Unknown=>"UNKNOWN",
        }
    }
    pub fn inspect(&self)->String{
        match self{
            Object::Integer(val)=>format!("{}",val),
            Object::Boolean(val)=>format!("{}",val),
            Object::Function { parameters, body, env }=>{
                format!("fn({}){{\n{}\n}}",parameters.iter().map(|x|x.string()).collect::<Vec<String>>().join(", "),body.string())
            },
            Object::Null=>format!("null"),
            Object::Return(val)=>format!("{}",val.inspect()),
            Object::String(s)=>format!("{s}"),
            Object::Builtin(f)=>format!("builtin function"),
            Object::Error(msg)=>format!("ERROR: {}",msg),
            Object::Unknown=>format!("unknown"),
        }
    }
}
