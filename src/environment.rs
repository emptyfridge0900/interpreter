use std::{collections::HashMap, rc::Rc};

use crate::object::Object;


#[derive(PartialEq,Eq,Clone,Debug)]
pub struct Environment{
    pub store:HashMap<String,Object>,
    pub outer:Option<Rc<Environment>>
}

impl Environment{
    pub fn new()->Environment{
        Environment{
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
                    let outer_val =self.outer.clone().unwrap().get(name);
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
    let mut env = new_environment();
    env.outer.replace(outer.into());
    return env;
}

fn new_environment()->Environment{
    let s:HashMap<String, Object> = HashMap::new();
    return Environment{
        store:s,
        outer:None
    }
}