use std::collections::HashMap;

use crate::{ast::{self, Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program, ReturnStatement, Statement}, lexer::Lexer, token::{self, Token}};


#[derive(PartialEq,Eq,Hash)]
pub enum Precedences{
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

type PrefixParseFn=fn (&mut Parser)->Option<Box<dyn Expression>>;
type InfixParseFn = fn (ex:dyn Expression)->Option<Box<dyn Expression>>;
pub struct Parser{
    l:Lexer,
    cur_token:Token,
    peek_token:Token,
    errors:Vec<String>,
    prefix_parse_fns:HashMap<String,PrefixParseFn>,
    infix_parse_fns:HashMap<String,InfixParseFn>
}
impl Parser{
    pub fn new(mut l:Lexer)->Parser{
        let current=l.next_token();
        let peek=l.next_token();
        let mut p = Parser { l, cur_token: current, peek_token:peek,errors:vec![],prefix_parse_fns:HashMap::new(),infix_parse_fns:HashMap::new() };

        p.register_prefix("ident".to_owned(), Parser::parse_identifier);
        p.register_prefix("int".to_owned(), Parser::parse_integer_literal);

        p
    }
    pub fn register_prefix(&mut self,token_type:String, func:PrefixParseFn){
        self.prefix_parse_fns.insert(token_type,func);
    }
    pub fn register_infix(&mut self,token_type:String,func:InfixParseFn){
        self.infix_parse_fns.insert(token_type, func);
    }
    pub fn next_token(&mut self){
        self.cur_token=self.peek_token.clone();
        self.peek_token=self.l.next_token();
    }
    pub fn parse_program(&mut self)->Program{
        let program=ast::Program::new();
        while self.cur_token != Token::EOF{
            let stmt = self.parse_statement();
            if stmt.is_some(){
                program.statements.borrow_mut().push(stmt.unwrap());
            }
            self.next_token()
        }
        program
    }
    pub fn parse_statement(&mut self)->Option<Box<dyn Statement>>{
        match &self.cur_token{
            Token::LET=>{
                let stmt= self.parse_let_statement();
                if stmt.is_none(){
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            },
            Token::RETURN=>{
                let stmt = self.parse_return_statement();
                if stmt.is_none(){
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            },
            _=>{
                let stmt = self.parse_expression_statement();
                if stmt.is_none(){
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            }
        }
    }
    //parseLetStatement 예제와 순서가 좀 다른게 진행... 나중에 에러날수도 있음
    //원래 순서는 LetStatement를 먼저 token과 만들고,나중에 name을 추가해주는 방식인데
    //여기서는 먼저 name을 만들고 나중에 LetStatement를 만든다
    pub fn parse_let_statement(&mut self)->Option<LetStatement>{

        let let_token= self.cur_token.clone();
        if !self.expect_peek(Token::IDENT("any letter for now".to_string())){
            return None
        }

        let identifier = match self.cur_token{
            Token::IDENT(ref s)=>{
                Identifier{token_type: self.cur_token.token_type(), token_value: s.clone()}
            },
            _=>{return None;}
        };

        //let name = ast::Identifier::new(self.cur_token.to_owned(),self.cur_token.to_string());
        let stmt =ast::LetStatement::new(let_token,identifier);
        
        if !self.expect_peek(Token::ASSIGN){
            return None
        }

        //todo
        while !self.cur_toekn_is(Token::SEMICOLON){
            self.next_token();
        }
        
        Some(stmt)
    }
    pub fn parse_return_statement(&mut self)->Option<ReturnStatement>{
        let save_token= self.cur_token.clone();
        let stmt = ReturnStatement::new(save_token);
        self.next_token();

        //todo
        while !self.cur_toekn_is(Token::SEMICOLON){
            self.next_token();
        }

        Some(stmt)
    }
    pub fn parse_expression_statement(&mut self)->Option<ExpressionStatement>{
        let exp =self.parse_expression(Precedences::LOWEST);
        let stmt = ExpressionStatement{token:self.cur_token.clone(),expression:exp};

        if self.peek_token_is(Token::SEMICOLON){
            self.next_token();
        }
        Some(stmt)
    }


     pub fn parse_expression(&mut self,precedence:Precedences)->Option<Box<dyn Expression>>{
        let prefix=self.prefix_parse_fns.get(&self.cur_token.token_type());
        if prefix.is_none(){
            return None
        }
        let func=prefix.unwrap();
        let left_exp= func(self);
        left_exp
    }
    pub fn parse_identifier(&mut self)->Option<Box<dyn Expression>>{
        Some(Box::new(Identifier::new(self.cur_token.token_type(), self.cur_token.token_value())))
    }
    pub fn parse_integer_literal(&mut self)->Option<Box<dyn Expression>>{
        let parse =self.cur_token.token_value().parse::<i64>();
        if parse.is_err(){
            self.errors.push(format!("could not parse {} as integer",self.cur_token.token_value()));
            return None;
        }
        let lit = IntegerLiteral::new(self.cur_token.clone(),parse.unwrap());

        Some(Box::new(lit))
    }

   
    pub fn parse_operator_expression(){

    }

    pub fn cur_toekn_is(&self, t: token::Token)->bool{
        self.cur_token == t
    }
    pub fn peek_token_is(&self, t: token::Token)->bool{
        self.peek_token == t
    }
    ///if the parameter match the peek_token, call next_token methos internally,
    ///which changes the current_token value then set the next peek_token
    pub fn expect_peek(&mut self, t: token::Token)->bool{
        
        //if both expect token and peek token are Token::IDENT type
        if let (Token::IDENT(x),Token::IDENT(y))=(t.clone(),self.peek_token.clone()){
            self.next_token();
            return true;
        }

        if self.peek_token_is(t.clone()){
            self.next_token();
            true
        }else{
            self.peek_error(t.clone());
            false
        }
    }

    pub fn errors(&self)->Vec<String>{
        self.errors.clone()
    }

    pub fn peek_error(&mut self, t:token::Token){
        let msg = format!("expected next token to be {:?}, got {:?} instead", t, self.peek_token);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use crate::{ast::{Expression, ExpressionStatement, Identifier, IntegerLiteral, Node, Program}, lexer::Lexer};

    use super::Parser;


    #[test]
    fn test_identifier_expression(){
        let input="foobar";
        let l=Lexer::new(input);
        let mut p= Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.borrow().len(),1);
        if program.statements.borrow().len()!=1{

        }
        let binding = program.statements.borrow();
        let stmt= binding[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>();
        if stmt.is_none(){
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
        let stmt = stmt.unwrap();
        let test = stmt.expression
        .as_ref().unwrap()
        .as_any()
        .downcast_ref::<Identifier>();

        if test.is_none(){

        }
        let ident=test.unwrap();
        if ident.token_value!="foobar"{
           println!("ident.value not foobar, got = {}",ident.token_literal()) ;
        }
        if ident.token_literal() !="foobar"{
            println!("ident.token_literal() not foobar, got ={}",ident.token_literal());
        }
        
    }

    #[test]
    fn test_integer_literal_expression(){

        let input="5;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program  = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.borrow().len(),1);
        if program.statements.borrow().len()!=1{

        }
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none(){
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
        let stmt = stmt.unwrap();
        let wrapped = stmt.expression
        .as_ref().unwrap()
        .as_any()
        .downcast_ref::<IntegerLiteral>();

        if wrapped.is_none(){
            panic!("exp not IntegerLiteral. got = None");
        }
        let literal = wrapped.unwrap();

        if literal.integer_value!=5{
            panic!("literal value not 5, got = {}",literal.integer_value);
        }
        if literal.token_literal() !="5"{
            panic!("literal.token_literal not \"5\" got = {}",literal.token_literal());
        }


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
}