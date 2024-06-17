use std::{ cell::{RefCell, RefMut}, collections::HashMap, fmt::Debug, hash::Hash, rc::Rc};
use crate::object::Object;


#[derive(Clone,Debug)]
pub struct Environment{
    env: Rc<RefCell<EnvData>>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.env, &other.env)
    }
}

impl Eq for Environment {}



impl Environment{
    pub fn new()->Environment{
        Environment{
            env:Rc::new(RefCell::new(EnvData::new()))
        }
    }
    pub fn get(&self, name: &String) -> Option<Object> {
        self.env.borrow().get(name.to_string())
    }

    pub fn set(&mut self,name:String,val:Object){
        self.env.borrow_mut().set(name, val);
    }
}



#[derive(Clone,Debug)]
pub struct EnvData{
    pub store:HashMap<String,Object>,
    pub outer:Option<Environment>
}


impl EnvData{
    pub fn new()->EnvData{
        EnvData{
            store:HashMap::new(),
            outer:None
        }
    }
    pub fn get(&self,name:String)->Option<Object>{
        let value = self.store.get(&name);
        match value {
            Some(v)=>Some(v.clone()),
            None=>{
                if self.outer.is_some(){
                    let outer_val =self.outer.clone().unwrap().get(&name);
                    return outer_val;
                }else{
                    return None;
                }
            }
        }
    }
    pub fn set(&mut self,name:String,val:Object)->Object{
        self.store.insert(name, val.clone());
        val
    }
}


pub fn new_enclosed_environment(outer:Environment)->Environment{
    let mut env = new_environment(outer);
    return env;
}

fn new_environment(outer:Environment)->Environment{
    let s:HashMap<String, Object> = HashMap::new();
    Environment{
        env:Rc::new(RefCell::new(EnvData{
            store:s,
            outer:Some(outer)
        }))
    }
}