use std::fmt::format;


#[derive(PartialEq)]
pub enum Object{
    Integer(i64),
    Boolean(bool),
    Null,

}

impl Object{
    pub fn get_type(&self)->&str{
        match self{
            Object::Integer(..)=>"INTEGER",
            Object::Boolean(..)=>"BOOLEAN",
            Object::Null=>"NULL",
            _=>"testing"
        }
    }
    pub fn inspect(&self)->String{
        match self{
            Object::Integer(val)=>format!("{}",val),
            Object::Boolean(val)=>format!("{}",val),
            Object::Null=>format!("null"),
            _=>"testing".to_string()
        }
    }
}
