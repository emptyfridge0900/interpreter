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
        builtins.functions.insert("first".to_string(), Builtins::first);
        builtins.functions.insert("last".to_string(), Builtins::last);
        builtins.functions.insert("rest".to_string(), Builtins::rest);
        builtins.functions.insert("push".to_string(), Builtins::push);
        builtins.functions.insert("puts".to_string(), Builtins::puts);
        builtins
    }
    pub fn get(&self,fn_name:String)->Object{
        let function = self.functions.get(&fn_name);
        if function.is_none(){
            println!("not a function");
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
            [Object::Array(a)] => Object::Integer(a.len() as i64),
            _=>new_error(format!("argument to 'len' not supported, got={:?}",args[0]))
        }
    }
    fn first(args:Vec<Object>)->Object{
        if args.len() !=1{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        if args[0].get_type()!="ARRAY"{
            return new_error(format!("argument to 'first' must be ARRAY, got={}",args[0].get_type()));
        }
        match &args[0]{
            Object::Array(arr)=>arr[0].clone(),
            _=>Object::Null
        }
    }
    fn last(args:Vec<Object>)->Object{
        if args.len() !=1{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        if args[0].get_type()!="ARRAY"{
            return new_error(format!("argument to 'last' must be ARRAY, got={}",args[0].get_type()));
        }
        match &args[0]{
            Object::Array(arr)=> if arr.len()>0 {arr[arr.len()-1].clone()}else {Object::Null},
            _=>Object::Null
        }
    }
    fn rest(args:Vec<Object>)->Object{
        if args.len() !=1{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        if args[0].get_type()!="ARRAY"{
            return new_error(format!("argument to 'rest' must be ARRAY, got={}",args[0].get_type()));
        }
        match &args[0]{
            Object::Array(arr)=> if arr.len()>0 {
                let ar=arr[1..].iter().map(|x|x.clone()).collect();
                Object::Array(ar)
            }else {
                Object::Null
            }
            ,
            _=>Object::Null
        }
    }
    fn push(args:Vec<Object>)->Object{
        if args.len() !=2{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        if args[0].get_type()!="ARRAY"{
            return new_error(format!("argument to 'rest' must be ARRAY, got={}",args[0].get_type()));
        }
        match &args[0]{
            Object::Array(arr)=>{
                //let length = arr.len();
                let mut ar=arr.iter().map(|x|x.clone()).collect::<Vec<Object>>();
                ar.push(args[1].clone());
                Object::Array(ar.into())
            },
            _=>Object::Null
        }
    }
    fn puts(args:Vec<Object>)->Object{
        for arg in args{
            println!("{}", arg.inspect());
        }
        Object::Null
    }
}