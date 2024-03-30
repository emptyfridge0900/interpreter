
#[derive(PartialEq,Eq,Clone,Debug)]
pub enum Object{
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Unknown
}

impl Object{
    pub fn get_type(&self)->&str{
        match self{
            Object::Integer(..)=>"INTEGER",
            Object::Boolean(..)=>"BOOLEAN",
            Object::Null=>"NULL",
            Object::Return(..)=>"RETURN_VALUE",
            Object::Error(..)=>"ERROR",
            Object::Unknown=>"UNKNOWN",
        }
    }
    pub fn inspect(&self)->String{
        match self{
            Object::Integer(val)=>format!("{}",val),
            Object::Boolean(val)=>format!("{}",val),
            Object::Null=>format!("null"),
            Object::Return(val)=>format!("{}",val.inspect()),
            Object::Error(msg)=>format!("ERROR: {}",msg),
            Object::Unknown=>format!("unknown"),
        }
    }
}
