use std::collections::HashMap;

use crate::object::Object;


pub struct Environment{
    pub store:HashMap<String,Object>
}
impl Environment{
    pub fn new()->Environment{
        Environment{
            store:HashMap::new()
        }
    }
    pub fn get(&self,name:String)->Option<Object>{
        let value = self.store.get(&name);
        match value {
            Some(v)=>Some(v.clone()),
            None=>None
        }
    }
    pub fn set(&mut self,name:String,val:Object)->Object{
        self.store.insert(name, val.clone());
        val
    }
}