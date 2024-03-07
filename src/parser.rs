use crate::{ast::{self, LetStatement, Program, Statement}, lexer::Lexer, token::{self, Token}};

pub struct Parser{
    l:Lexer,
    cur_token:Token,
    peek_token:Token
}
impl Parser{
    pub fn new(mut l:Lexer)->Parser{
        let current=l.next_token();
        let peek=l.next_token();
        Parser { l, cur_token: current, peek_token:peek }
    }
    pub fn next_token(&mut self){
        self.cur_token=self.peek_token.clone();
        self.peek_token=self.l.next_token();
    }
    pub fn parse_program(&mut self)->Program{
        let program=ast::Program::new();
        while self.cur_token.token_type != token::EOF{
            let stmt = self.parse_statement();
            if stmt.is_some(){
                program.statements.borrow_mut().push(stmt.unwrap());
            }
            self.next_token()
        }
        program
    }
    pub fn parse_statement(&mut self)->Option<Box<dyn Statement>>{
        match &self.cur_token.token_type[..]{
            token::LET=>{
                let stmt= self.parse_let_statement();
                if stmt.is_none(){
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            },
            _=>None
        }
    }
    //parseLetStatement 예제와 순서가 좀 다른게 진행... 나중에 에러날수도 있음
    //원래 순서는 LetStatement를 먼저 token과 만들고,나중에 name을 추가해주는 방식인데
    //여기서는 먼저 name을 만들고 나중에 LetStatement를 만든다
    pub fn parse_let_statement(&mut self)->Option<LetStatement>{

        let save_token= self.cur_token.clone();
        if !self.expect_peek(token::IDENT.to_owned()){
            return None
        }
        let name = ast::Identifier::new(self.cur_token.clone(),self.cur_token.literal.clone());
        let stmt =ast::LetStatement::new(save_token,name);
        
        if !self.expect_peek(token::ASSIGN.to_owned()){
            return None
        }

        //todo
        while self.cur_toekn_is(token::SEMICOLON.to_owned()){
            self.next_token();
        }
        
        Some(stmt)
    }
    pub fn parse_identifier(){

    }
    pub fn parse_expression(){

    }
    pub fn parse_operator_expression(){

    }

    pub fn cur_toekn_is(&self, t: token::TokenType)->bool{
        self.cur_token.token_type == t
    }
    pub fn peek_token_is(&self, t: token::TokenType)->bool{
        self.peek_token.token_type == t
    }
    ///if the parameter match the peek_token, call next_token methos internally,
    ///which changes the current_token value then set the next peek_token
    pub fn expect_peek(&mut self, t: token::TokenType)->bool{
        if self.peek_token_is(t){
            self.next_token();
            true
        }else{
            false
        }
    }

}