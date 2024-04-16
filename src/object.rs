use std::{collections::HashMap, fmt::format, rc::Rc,hash::Hash};

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
    Array(Box<[Object]>),
    Hash(HashMap<Object,Object>),
    Unknown
}
impl Hash for Object{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
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
            Object::Array(_)=>"ARRAY",
            Object::Hash(_)=>"HASH",
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
            Object::Array(a)=>{
                format!("[{}]",a.iter().map(|x|x.inspect()).collect::<Vec<String>>().join(", "))
            },
            Object::Hash(h)=>{
                let pair =h.iter().map(|p|format!("{}:{}",p.0.inspect(),p.1.inspect())).collect::<Vec<String>>().join(", ");
                format!("{{{pair}}}")
            },
            Object::Error(msg)=>format!("ERROR: {}",msg),
            Object::Unknown=>format!("unknown"),
        }
    }
}
// #[cfg(test)]
// mod tests {

//     use std::hash::{DefaultHasher, Hash};

//     use super::Object;

//     #[test]
//     fn test_string_hash_key(){
//         let hello1= Object::String("Hello World".to_string());
//         let hello2= Object::String("Hello World".to_string());
//         let diff1= Object::String("My name is johnny".to_string());
//         let diff2= Object::String("My name is johnny".to_string());

//         let mut hasher = DefaultHasher::new();
//         diff1.hash(&mut hasher);
//         if hello1.hash_key() != hello2.hash_key(){
//             println!("strings with same content have different hash keys");
//         }
//         if diff1.hash_key() != diff2.hash_key(){
//             println!("strings with same content have different hash keys");
//         }
//         if hello1.hash_key() == diff1.hash_key(){
//             println!("strings with different content have same hash keys");
//         }
        
//     }
// }
