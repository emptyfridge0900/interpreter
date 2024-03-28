
#[derive(PartialEq,Eq)]
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
        }
    }
    pub fn inspect(&self)->String{
        match self{
            Object::Integer(val)=>format!("{}",val),
            Object::Boolean(val)=>format!("{}",val),
            Object::Null=>format!("null"),
        }
    }
}
