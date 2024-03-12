use std::collections::HashMap;

use crate::{ast::{self, Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement}, lexer::Lexer, token::{self, Token}};


#[derive(PartialEq,Eq,Hash,PartialOrd)]
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
type InfixParseFn = fn (&mut Parser, ex:Option<Box<dyn Expression>>)->Option<Box<dyn Expression>>;
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
        p.register_prefix(Token::BANG.token_type(), Parser::parse_prefix_expression);
        p.register_prefix(Token::MINUS.token_type(), Parser::parse_prefix_expression);

        p.register_infix(Token::PLUS.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::MINUS.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::SLASH.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::ASTERISK.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::EQ.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::NOT_EQ.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::LT.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::GT.token_type(), Parser::parse_infix_expression);

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

        //If the peekToken is a token.SEMICOLON, we advance so it’s the curToken. 
        //If it’s not there, that’s okay too, we don’t add an error to the parser if it’s not there. 
        //That’s because we want expression statements to have optional semicolons
        //p.55
        if self.peek_token_is(Token::SEMICOLON){
            self.next_token();
        }
        Some(stmt)
    }


    pub fn parse_expression(&mut self,precedence:Precedences)->Option<Box<dyn Expression>>{
        let prefix=self.prefix_parse_fns.get(&self.cur_token.token_type());
        if prefix.is_none(){
            self.no_prefix_parse_fn_error(self.cur_token.clone());
            return None
        }
        let prefix=prefix.unwrap();
        let mut left_exp= prefix(self);

        while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek_precedence(){
            let infix= self.infix_parse_fns.get(&self.peek_token.token_type());
            if infix.is_none(){
                return left_exp;
            }
            self.next_token();

            //self.next_token() borrow mutably, so cannot continu using infix variable.
            //let infix=infix.unwrap();
            let infix= self.infix_parse_fns.get(&self.cur_token.token_type()).unwrap();
            left_exp=infix(self,left_exp);
        }

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

    //All it does is checking whether we have a parsing function associated with p.curToken.Type in the prefix position.
    //If we do, it calls this parsing function, if not, it returns nil. 
    //Which it does at the moment, since we haven’t associated any tokens with any parsing functions yet.
    pub fn parse_prefix_expression(&mut self)->Option<Box<dyn Expression>>{
        let mut expression=PrefixExpression::new(self.cur_token.clone(),self.cur_token.token_value(),None);
        self.next_token();
        let expr = self.parse_expression(Precedences::PREFIX);
        expression.right.replace(expr.unwrap());
        Some(Box::new(expression))
    }
    pub fn parse_infix_expression(&mut self,left:Option<Box<dyn Expression>>)->Option<Box<dyn Expression>>{
        let mut expression = InfixExpression::new(self.cur_token.clone(),left,self.cur_token.token_value(),None);
        let precedence = self.current_precedence();
        self.next_token();
        let right= self.parse_expression(precedence);
        expression.right.replace(right.unwrap());
        Some(Box::new(expression))
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

    pub fn peek_precedence(&self)->Precedences{
        self.peek_token.precedence()
    }

    pub fn current_precedence(&self)->Precedences{
        self.cur_token.precedence()
    }
    fn no_prefix_parse_fn_error(&mut self, t:Token){
        self.errors.push(format!("no prefix parse function for {} found",t.token_type()));
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use crate::{ast::{Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, Node, PrefixExpression, Program}, lexer::Lexer};

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

    #[test]
    fn test_parsing_prefix_expression(){
        let prefix_test:Vec<(&str,&str,i64)> =vec![
            ("!5;","!",5),
            ("-15;","-",15),
            ];

        for tt in prefix_test{
            let l=Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.statements.borrow().len()!=1{
                println!("program.statements does not contain {} statements. got = {}",1,program.statements.borrow().len());
            }
            let binding = program.statements.borrow();
            let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
            if stmt.is_none(){
                println!("program.statement[0] is not ExpresstionStatement.");
            }

            let exp = stmt.unwrap()
            .expression.as_ref().unwrap().as_any().downcast_ref::<PrefixExpression>();

            if exp.is_none(){
                println!("stmt is not PrefixExpression.");
            }
            if exp.unwrap().operator!=tt.1{
                println!("exp.operator is not {}",tt.1);
            }
            let exp=exp.unwrap();
            if !test_integer_literal(exp.right.as_ref(),tt.2){
                return;
            }

        }
    }
    #[test]
    fn test_parsing_infix_expressions(){
        let infix_tests:Vec<(&str,i64,&str,i64)> = vec![
            ("5+5;",5,"+",5),
            ("5-5;",5,"-",5),
            ("5*5;",5,"*",5),
            ("5/5;",5,"/",5),
            ("5>5;",5,">",5),
            ("5<5;",5,"<",5),
            ("5==5;",5,"==",5),
            ("5!=5;",5,"!=",5),
        ];
        for tt in infix_tests{
            let l=Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

             if program.statements.borrow().len()!=1{
                println!("program.statements does not contain {} statements. got = {}",1,program.statements.borrow().len());
            }
            let binding = program.statements.borrow();
            let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
            if stmt.is_none(){
                println!("program.statement[0] is not ExpresstionStatement.");
            }

            let exp = stmt.unwrap()
            .expression.as_ref().unwrap().as_any().downcast_ref::<InfixExpression>();

            assert_eq!(exp.is_some(),true);
            if exp.is_none(){
                println!("stmt is not InfixExpression.");
            }
            let exp=exp.unwrap();
            if !test_integer_literal(exp.left.as_ref(), tt.1){
                return
            }
            if exp.operator != tt.2{
                println!("exp.operator is not {} got={}",tt.2,exp.operator);
            }
            if !test_integer_literal(exp.right.as_ref(), tt.3){
                return
            }

        }

    }
    #[test]
    fn test_operator_precedence_parsing(){
        let tests:Vec<(&str,&str)> =vec![
            ("-a * b","((-a) * b)"),
            ("!-a","(!(-a))"),
            ("a + b + c","((a + b) + c)"),
            ("a + b - c","((a + b) - c)"),
            ("a * b * c","((a * b) * c)"),
            ("a * b / c","((a * b) / c)"),
            ("a + b / c","(a + (b / c))"),
            ("a + b * c + d / e - f","(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5","(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4","((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4","((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5","((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
        ];
        for tt in tests{
            let l=Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.string();
            if actual != tt.1{
                assert_eq!(tt.1,actual);
                println!("expected={},got={}",tt.1,actual);
            }
        }

    }

    fn test_integer_literal(il:Option<&Box<dyn Expression>>,value:i64)->bool{
        let integ = il.as_ref().unwrap().as_any().downcast_ref::<IntegerLiteral>();
        if integ.is_none(){
            println!("il not IntegerLiteral");
            return false;
        }
        let integ=integ.unwrap();
        if integ.integer_value!=value{
            println!("integ.value not {}. got ={}",value,integ.integer_value);
            return false;
        }
        if integ.token_literal() != format!("{}",value){
            println!("integ.token_literal() not {}. got={}",value,integ.token_literal());
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
            println!("parser error: {}",msg);
        }
    }
}