use std::{collections::HashMap, rc::Rc};

use crate::{evaluator::new_error, object::Object};


pub struct Builtins{
    functions:HashMap<String,fn(args:Vec<Object>) -> Object>
}
impl Builtins{
    pub fn new()->Builtins{
        let mut builtins =Builtins{
            functions:HashMap::new()
        };
        builtins.functions.insert("len".to_string(), Builtins::len);
        builtins
    }
    pub fn get(&self,fn_name:String)->Object{
        let function = self.functions.get(&fn_name);
        if function.is_none(){
            println!("");
            return Object::Null;
        }
        Object::Builtin(*function.unwrap())
    }

    fn len(args:Vec<Object>)->Object{
        if args.len() !=1{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        match &args[..]{
            [Object::String(v)] => Object::Integer(v.len() as i64),
            _=>new_error(format!("argument to 'len' not supported, got={:?}",args[0]))
        }
    }
}