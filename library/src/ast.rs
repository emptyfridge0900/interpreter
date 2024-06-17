use std::{any::Any, cell::RefCell, clone, collections::HashMap, hash::{Hash, Hasher}, rc::Rc};

use crate::token::{self, Token};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Rc<Expression>),
}


#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Program{
    pub statements:Vec<Statement>
}
impl Program {
    pub fn new() -> Program {
        Program {
            statements: vec![],
        }
    }
    pub fn string(&self)->String{
        let ret: Vec<String> = self
            .statements
            .iter()
            .map(|x| x.string())
            .collect();
        ret.join("")
    }
}

#[derive(PartialEq, Eq, Clone, Debug,Hash)]
pub enum Statement{
    Let { 
        token: Token, 
        ident: Identifier, 
        value: Expression 
    },
    Return{
        token:Token, 
        value:Expression
    },
    Expression{
        token:Token,
        expression:Expression
    },
    Block{
        token:Token,
        statements:Vec<Statement>
    }
}
impl Statement{
    pub fn string(&self)->String{
        match self{
            Statement::Let { token, ident, value }=> format!("{} {} = {};",token, ident.name, value.string() ),
            Statement::Return { token, value }=>format!("{} {};",token,value.string()),
            Statement::Expression { token, expression }=>format!("{}",expression.string()),
            Statement::Block { token, statements }=>format!("{}",statements.iter().map(|x|x.string()).collect::<Vec<String>>().join(", "))
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression{
    Identifier(Identifier),
    IntegerLiteral{//token is INT
        value:i64
    },
    Boolean{
        value:bool
    },
    Prefix{
        token:Token,
        operator:String,
        right:Rc<Expression>
    },
    Infix{
        token:Token,
        left:Rc<Expression>,
        operator:String,
        right:Rc<Expression>
    },
    If{
        condition:Rc<Expression>,
        consequence:Rc<Statement>,//blockstatement
        alternative:Option<Rc<Statement>>,//blockstatment
    },
    FunctionLiteral{
        token:Token,
        parameters:Vec<Identifier>,//identifier
        body:Rc<Statement>
    },
    Call{
        token:Token,
        function:Rc<Expression>,
        arguments:Vec<Expression>
    },
    StringLiteral{
        token:Token,
        value:String
    },
    ArrayLiteral{
        token:Token,
        elements:Vec<Expression>
    },
    Index{
        left:Rc<Expression>,
        index:Rc<Expression>
    },
    HashLiteral{
        pairs:HashMap<Expression,Expression>
    },
    Error
}
impl Hash for Expression{
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Expression{
    pub fn string(&self)->String{
        match self{
            Expression::Identifier(ident)=>ident.string(),
            Expression::IntegerLiteral { value }=>value.to_string(),
            Expression::StringLiteral { token, value }=>token.token_value(),
            Expression::Boolean {  value }=>value.to_string(),
            Expression::Prefix { token, operator, right }=>format!("({}{})",operator,right.string()),
            Expression::Infix { token, left, operator, right }=>format!("({} {} {})",left.string(),operator,right.string()),
            Expression::If { condition, consequence, alternative }=>{
                if alternative.is_none(){
                    format!("if{} {}",condition.string(),consequence.string())
                }else{
                    format!("if{} {}else {}",
                    condition.string(),
                    consequence.string(),
                    alternative.as_ref().unwrap().string())
                }
            },
            Expression::FunctionLiteral {token, parameters, body }=>
            format!("{}({}){}",token.token_value(),parameters.iter().map(|x|x.string()).collect::<Vec<String>>().join(", "),body.string()),
            Expression::ArrayLiteral { token, elements } =>
            format!("[{}]",elements.iter().map(|x|x.string()).collect::<Vec<String>>().join(", ")),
            Expression::Call { token, function, arguments }=>
            format!("{}({})",function.string(),arguments.iter().map(|x|x.string()).collect::<Vec<String>>().join(", ")),
            Expression::Index { left, index }=>format!("({}[{}])",left.string(),index.string()),
            Expression::HashLiteral { pairs }=>{
                let string = pairs.iter().map(|x|x.0.string()).collect::<Vec<String>>().join(", ");
                format!("{{{}}}",string)
            }
            Expression::Error=>"".to_owned()
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug,Hash)]
pub struct Identifier{
    pub name:String
}
impl Identifier{
    pub fn string(&self)->String{
        self.name.to_owned()
    }
}


#[cfg(test)]
mod tests {
    use crate::{ast::{Program, Statement}, token::Token};
    use super::{Expression, Identifier};

    #[test]
    pub fn test_string(){
        let mut program= Program{statements:vec![]};
        let mut let_statement= Statement::Let{
            token:Token::LET,
            ident:Identifier{name:"myVar".to_string()},
            value: Expression::Identifier(Identifier{name:String::from("anotherVar")}),
        };

        program.statements.push(let_statement);
        assert_eq!(program.string(), "LET myVar = anotherVar;");
    }

}