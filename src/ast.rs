use std::{any::Any, cell::RefCell};

use crate::token::{self, Token};

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: RefCell<Vec<Box<dyn Statement>>>,
}
impl Program {
    pub fn new() -> Program {
        Program {
            statements: RefCell::new(vec![]),
        }
    }
}
impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.borrow().len() > 0 {
            self.statements.borrow()[0].as_ref().token_literal().clone()
        } else {
            "".to_string()
        }
    }
    fn string(&self) -> String {
        let ret: Vec<String> = self
            .statements
            .borrow()
            .iter()
            .map(|x| x.string())
            .collect();
        ret.join("")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}


//statement
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}
impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> LetStatement {
        LetStatement {
            token,
            name,
            value: None,
        }
    }
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.token_value()
    }
    fn string(&self) -> String {
        let mut vec=vec![];
        vec.push(self.token_literal());
        vec.push(" ".to_owned());
        vec.push(self.name.string());
        vec.push(" = ".to_owned());
        if self.value.is_some(){
            vec.push(self.value.as_ref().unwrap().string());
        }
        vec.push(";".to_owned());
        vec.join("")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}



pub struct ReturnStatement {
    token: token::Token,
    pub return_value: Option<Box<dyn Expression>>,
}
impl ReturnStatement {
    pub fn new(token: Token) -> ReturnStatement {
        ReturnStatement {
            token,
            return_value: None,
        }
    }
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.token_type()
    }
    fn string(&self) -> String {
        let mut st=String::from(self.token_literal());
        st.push(' ');
        if self.return_value.is_some(){
            st.push_str(&self.return_value.as_ref().unwrap().string());
        }
        st.push(';');
        st.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<dyn Expression>>,
}
impl ExpressionStatement {
    pub fn new(token:token::Token) -> ExpressionStatement {
        ExpressionStatement {token,expression:None}
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.token_value()
    }
    fn string(&self) -> String {
        if self.expression.is_some(){
            return self.expression.as_ref().unwrap().string().clone()
        }
        String::new()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

pub struct BlockStatement{
    pub token:Token,
    pub statements:Vec<Box<dyn Statement>>
}
impl BlockStatement{
    pub fn new(token:Token)->BlockStatement{
        BlockStatement{
            token,
            statements:vec![]
        }
    }
}
impl Node for BlockStatement{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        let ret: Vec<String> = self
            .statements
            .iter()
            .map(|x| x.string())
            .collect();
        ret.join("")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for BlockStatement{
    fn statement_node(&self) {
    }
}

//expression

#[derive(Clone)]
pub struct Identifier {
    pub token_type: String,
    pub token_value: String,
}
impl Identifier {
    pub fn new(token_type: String, token_value: String) -> Identifier {
        Identifier { token_type, token_value }
    }
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token_value.clone()
    }
    fn string(&self) -> String {
        self.token_value.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier{
    fn expression_node(&self) {
        todo!()
    }
}


pub struct IntegerLiteral{
    pub token: token::Token,
    pub integer_value: i64,
}
impl IntegerLiteral{
    pub fn new(token:Token,value:i64)->IntegerLiteral{
        IntegerLiteral{
            token,
            integer_value:value
        }
    }
}
impl Node for IntegerLiteral{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        self.integer_value.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for IntegerLiteral{
    fn expression_node(&self) {
    }
}

pub struct PrefixExpression{
    pub token:Token,
    pub operator:String,
    pub right:Option<Box<dyn Expression>>
}
impl PrefixExpression{
    pub fn new(token:Token,operator:String,right:Option<Box<dyn Expression>>)->PrefixExpression{
        PrefixExpression{
            token,
            operator,
            right
        }
    }
}
impl Node for PrefixExpression{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        let str = self.right.as_ref().unwrap().string().clone();
        format!("({}{})",self.operator, str)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for PrefixExpression{
    fn expression_node(&self) {
    }
}

pub struct InfixExpression{
    pub token:Token,
    pub left:Option<Box<dyn Expression>>,
    pub operator:String,
    pub right:Option<Box<dyn Expression>>
}
impl InfixExpression{
    pub fn new(token:Token,left:Option<Box<dyn Expression>>,operator:String,right:Option<Box<dyn Expression>>)->InfixExpression{
        InfixExpression{
            token,
            left,
            operator,
            right
        }
    }
}
impl Node for InfixExpression{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        let left = self.left.as_ref().unwrap().string().clone();
        let right = self.right.as_ref().unwrap().string().clone();
        format!("({} {} {})",left,self.operator, right)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for InfixExpression{
    fn expression_node(&self) {
    }
}

pub struct Boolean{
    pub token:Token,
    pub boolean_value:bool
}
impl Boolean{
    pub fn new(token:Token,boolean_value:bool)->Boolean{
        Boolean{
            token,
            boolean_value
        }
    }
}
impl Node for Boolean{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        self.boolean_value.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for Boolean{
    fn expression_node(&self) {
    }
}


pub struct IfExpression{
    pub token:Token,
    pub condition: Option<Box<dyn Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>
}
impl IfExpression{
    pub fn new(token:Token,condition:Option<Box<dyn Expression>>,consequence: Option<BlockStatement>,alternative: Option<BlockStatement>)->IfExpression{
        IfExpression{
            token,
            condition,
            consequence,
            alternative
        }
    }
}
impl Node for IfExpression{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        if self.alternative.is_none(){
            format!("if {} {}",self.condition.as_ref().unwrap().string(),self.consequence.as_ref().unwrap().string().clone())
        }else{
            format!("if {} {} else {}",self.condition.as_ref().unwrap().string(),self.consequence.as_ref().unwrap().string().clone(), self.alternative.as_ref().unwrap().string().clone())
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for IfExpression{
    fn expression_node(&self) {
    }
}

pub struct FunctionLiteral{
    pub token:Token,
    pub parameters:Vec<Identifier>,
    pub body:Option<BlockStatement>
}
impl FunctionLiteral{
    pub fn new(token:Token,parameters:Vec<Identifier>,body:Option<BlockStatement>)->FunctionLiteral{
        FunctionLiteral{
            token,
            parameters,
            body
        }
    }
}
impl Node for FunctionLiteral{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        let param=self.parameters.iter()
            .map(|x| x.string())
            .collect::<Vec<String>>();
        format!("{}({}) {}",self.token_literal(),param.join(", "), self.body.as_ref().unwrap().string())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for FunctionLiteral{
    fn expression_node(&self) {
    }
}


pub struct CallExpression{
    pub token:Token,
    pub function:Box<dyn Expression>,
    pub arguments:Vec<Box<dyn Expression>>
}
impl CallExpression{
    pub fn new(token:Token,function:Box<dyn Expression>,arguments:Vec<Box<dyn Expression>>)->CallExpression{
        CallExpression{
            token,
            function,
            arguments
        }
    }
}
impl Node for CallExpression{
    fn token_literal(&self) -> String {
        self.token.token_value()
    }

    fn string(&self) -> String {
        format!("{}({})",self.function.string(),self.arguments.iter().map(|x|x.string()).collect::<Vec<String>>().join(", "))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for CallExpression{
    fn expression_node(&self) {
    }
}
#[cfg(test)]
mod tests {
    use crate::{ast::Node, token::Token};
    use super::{Expression, Identifier, LetStatement, Program};

    #[test]
    pub fn test_string(){
        let program= Program::new();
        let mut let_statement= LetStatement::new(
            Token::LET,
            Identifier::new("ident".to_owned(), "myVar".to_owned())
        );
        let boxed:Box<dyn Expression> = Box::new(Identifier::new("ident".to_owned(),"anotherVar".to_owned()));
        let_statement.value.replace(boxed);
        program.statements.borrow_mut().push(Box::new(let_statement));
        assert_eq!(program.string(), "let myVar = anotherVar;");

    }

}