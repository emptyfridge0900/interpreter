
#[derive(PartialEq,Eq,Clone,Debug)]
pub enum Object{
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Unkown
}

impl Object{
    pub fn get_type(&self)->&str{
        match self{
            Object::Integer(..)=>"INTEGER",
            Object::Boolean(..)=>"BOOLEAN",
            Object::Null=>"NULL",
            Object::Return(..)=>"RETURN_VALUE",
            Object::Unkown=>"UNKOWN",
        }
    }
    pub fn inspect(&self)->String{
        match self{
            Object::Integer(val)=>format!("{}",val),
            Object::Boolean(val)=>format!("{}",val),
            Object::Null=>format!("null"),
            Object::Return(val)=>format!("{}",val.inspect()),
            Object::Unkown=>format!("unkown"),
        }
    }
}
