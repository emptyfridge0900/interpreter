use std::{cell::RefCell, rc::Rc};

use crate::{evaluator::new_error, object::Object};


pub struct Builtins<'a>{
    output:Rc<RefCell<dyn FnMut(&str) + 'a>>
}
impl<'a> Builtins<'a>{
    pub fn new(output:Rc<RefCell<dyn FnMut(&str) + 'a>>)->Builtins<'a>{
        let builtins =Builtins{
            output
        };

        builtins
    }
    pub fn get(&self,fn_name:&str)->Object{
        match fn_name{
            "len"=>Object::Builtin("len".to_string()),
            "first"=>Object::Builtin("first".to_string()),
            "last"=>Object::Builtin("last".to_string()),
            "rest"=>Object::Builtin("rest".to_string()),
            "push"=>Object::Builtin("push".to_string()),
            "puts"=>Object::Builtin("puts".to_string()),
            _=>Object::Null
        }

    }
    pub fn call(&self, name:&str, args:Vec<Object>)->Object{
        match name{
            "len" => self.len(args),
            "first" => self.first(args),
            "last" => self.last(args),
            "rest" => self.rest(args),
            "push" => self.push(args),
            "puts" => self.puts(args),
            _ => Object::Null
        }
    }

    fn len(&self,args:Vec<Object>)->Object{
        if args.len() !=1{
            return new_error(format!("wrong number of arguments. got={}, want=1",args.len()));
        }
        match &args[..]{
            [Object::String(v)] => Object::Integer(v.len() as i64),
            [Object::Array(a)] => Object::Integer(a.len() as i64),
            _=>new_error(format!("argument to 'len' not supported, got={:?}",args[0]))
        }
    }
    fn first(&self, args:Vec<Object>)->Object{
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
    fn last(&self, args:Vec<Object>)->Object{
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
    fn rest(&self, args:Vec<Object>)->Object{
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
    fn push(&self, args:Vec<Object>)->Object{
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
    pub fn puts(&self, args:Vec<Object>)->Object{
        if args.len()==0{
            return Object::Null
        }
        let ret=args.iter().map(|x|x.inspect()).collect::<Vec<String>>().join(" ");

        self.output.borrow_mut()(&ret);
        Object::String(ret)
    }
}