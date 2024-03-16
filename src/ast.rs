use std::{any::Any, borrow::Borrow, cell::RefCell, clone, path::Prefix};

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
    return_value: Option<Box<dyn Expression>>,
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
    use std::any::Any;

    use crate::{ast::Node, lexer::Lexer, parser::Parser, token::{self, Token}};

    use super::{Expression, Identifier, LetStatement, Program, ReturnStatement};


    #[test]
    fn test_let_statements(){
        let input = "
        let x = 5;
let  y= 10;
let foobar = 838383;
";
        let mut l=Lexer::new(input);
        let mut p= Parser::new(l);
        
        let program:Program= p.parse_program();
        check_parser_errors(&p);

        //if program == nil
        if program.statements.borrow().len()==0{
            panic!("part_program() return 0 statements");
            return;
        }

        if program.statements.borrow().len()!=3{
            panic!("program.statements does not contain 3 statements. got={}",program.statements.borrow().len());
            return;
        }
        let tests:Vec<String> =vec![
            "x".to_owned(),
            "y".to_owned(),
            "foobar".to_owned(),
        ];
        for (i,tt) in tests.into_iter().enumerate(){
            let stmt=program.statements.borrow();
            if !test_let_statement(stmt[i].as_ref().as_any(),tt){
                return;
            }
        }

    }

    fn test_let_statement(s:&dyn Any,name:String)->bool{


        let statement =s.downcast_ref::<LetStatement>();
        if statement.is_none(){
            return false;
        }
        let let_stmt : &LetStatement = statement.unwrap();
        if let_stmt.token_literal() != "let"{
            println!("TokenLiteral not 'let'. got={}",let_stmt.token_literal());
            return false;
        }
        if let_stmt.name.token_value != name{
            println!("let statement.name.value not {} got={:?}",name, let_stmt.name.token_value);
            return false;
        }
        if let_stmt.name.token_literal() != name{
            println!("let statement.name.token_literal not {} got={}",name, let_stmt.name.token_literal());
            return false;
        }
        true
    }

    fn check_parser_errors(p:&Parser){
        let errors= p.errors();
        if errors.len()==0{
            return
        }

        println!("parser has {} errors", errors.len());
        for msg in errors{
            println!("{}",msg);
        }
    }
    #[test]
    fn test_return_statements(){
        let input = "
        return 5;
return 10;
return 993322;
";
        let mut l=Lexer::new(input);
        let mut p= Parser::new(l);
        
        let program:Program= p.parse_program();
        check_parser_errors(&p);


        assert_eq!(program.statements.borrow().len(),3);
        if program.statements.borrow().len()!=3{
            println!("program.statements does not contain 3 statements. got={}",program.statements.borrow().len());
            return;
        }
        
        for stmt in program.statements.borrow().iter(){
            let return_statement =stmt.as_any().downcast_ref::<ReturnStatement>();
            if return_statement.is_none(){
                println!("statement not ReturnStatement");
                continue;
            }
            let return_stmt  = return_statement.unwrap();
            if return_stmt.token_literal() != "return"{
                println!("return_stmt.token_literal not 'return', got ={}",return_stmt.token_literal());
            }
        }

    }


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