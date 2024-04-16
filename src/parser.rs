use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{
        self,  Expression,  Identifier,  Program,  Statement
    },
    lexer::Lexer,
    token::{self, Token},
};

#[derive(PartialEq, Eq, Hash, PartialOrd)]
pub enum Precedences {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
    INDEX,
}

type PrefixParseFn = fn(&mut Parser) -> Expression;
type InfixParseFn = fn(&mut Parser, ex: Expression) -> Expression;
pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<String, PrefixParseFn>,
    infix_parse_fns: HashMap<String, InfixParseFn>,

    //for testing purpose
    cur: String,
    peek: String,
}
impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        let current = l.next_token();
        let peek = l.next_token();
        let mut p = Parser {
            l,
            cur_token: current,
            peek_token: peek,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            cur: "".to_string(),
            peek: "".to_string(),
        };

        p.register_prefix("ident".to_owned(), Parser::parse_identifier);
        p.register_prefix("int".to_owned(), Parser::parse_integer_literal);
        p.register_prefix("string".to_string(), Parser::parse_string_literal);
        p.register_prefix(Token::BANG.token_type(), Parser::parse_prefix_expression);
        p.register_prefix(Token::MINUS.token_type(), Parser::parse_prefix_expression);
        p.register_prefix(Token::TRUE.token_type(), Parser::parse_boolean);
        p.register_prefix(Token::FALSE.token_type(), Parser::parse_boolean);
        p.register_prefix(Token::LPAREN.token_type(), Parser::parse_grouped_expression);
        p.register_prefix(Token::IF.token_type(), Parser::parse_if_expression);
        p.register_prefix(Token::FUNCTION.token_type(),Parser::parse_functional_literal);
        p.register_prefix(Token::LBRACKET.token_type(),Parser::parse_array_literal);
        p.register_prefix(Token::LBRACE.token_type(),Parser::parse_hash_literal);

        p.register_infix(Token::PLUS.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::MINUS.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::SLASH.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::ASTERISK.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::EQ.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::NOT_EQ.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::LT.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::GT.token_type(), Parser::parse_infix_expression);
        p.register_infix(Token::LPAREN.token_type(), Parser::parse_call_expression);
        p.register_infix(Token::LBRACKET.token_type(), Parser::parse_index_expression);



        p
    }
    pub fn register_prefix(&mut self, token_type: String, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }
    pub fn register_infix(&mut self, token_type: String, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
        //testing
        self.cur = self.cur_token.token_value().clone();
        self.peek = self.peek_token.token_value().clone();
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = ast::Program::new();
        while !self.cur_toekn_is(Token::EOF) {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                program.statements.push(stmt.unwrap());
            }
            self.next_token()
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.cur_token {
            Token::LET => {
                let stmt = self.parse_let_statement();
                if stmt.is_none() {
                    return None;
                }
                stmt
            }
            Token::RETURN => {
                let stmt = self.parse_return_statement();
                if stmt.is_none() {
                    return None;
                }
                stmt
            }
            _ => {
                let stmt = self.parse_expression_statement();
                if stmt.is_none() {
                    return None;
                }
                stmt
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();
        if !self.expect_peek(Token::IDENT("any letter for now".to_string())) {
            return None;
        }

        let identifier = match self.cur_token {
            Token::IDENT(ref s) => Identifier {
                name: s.clone(),
            },
            _ => {
                return None;
            }
        };

        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }
        self.next_token();

        let value=self.parse_expression(Precedences::LOWEST);
        let mut stmt = ast::Statement::Let{token:let_token, ident:identifier,value};

        if self.peek_token_is(Token::SEMICOLON){
            self.next_token();
        }
        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let save_token = self.cur_token.clone();
        self.next_token();
        
        let return_value=self.parse_expression(Precedences::LOWEST);
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        let mut stmt = Statement::Return{token:save_token,value:return_value};

        Some(stmt)
    }
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let exp = self.parse_expression(Precedences::LOWEST);
        let stmt = Statement::Expression{
            token: self.cur_token.clone(),
            expression: exp,
        };

        //If the peekToken is a token.SEMICOLON, we advance so it’s the curToken.
        //If it’s not there, that’s okay too, we don’t add an error to the parser if it’s not there.
        //That’s because we want expression statements to have optional semicolons
        //p.55
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Some(stmt)
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let cur_token=self.cur_token.clone();
        self.next_token();
        let mut statements=vec![];
        while !self.cur_toekn_is(Token::RBRACE) && !self.cur_toekn_is(Token::EOF) {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                statements.push(stmt.unwrap());
            }
            self.next_token();
        }
        let block =Statement::Block{token:self.cur_token.clone(),statements:statements};
        Some(block)
    }





    fn parse_expression(&mut self, precedence: Precedences) -> Expression {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type());
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.cur_token.clone());
            return Expression::Error;//원본에서는 nil을 return함
        }
        let prefix = prefix.unwrap();
        let mut left_exp = prefix(self);

        while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fns.get(&self.peek_token.token_type());
            if infix.is_none() {
                return left_exp;
            }
            self.next_token();

            //self.next_token() borrow mutably, so cannot continu using infix variable.
            //let infix=infix.unwrap();
            let infix = self
                .infix_parse_fns
                .get(&self.cur_token.token_type())
                .unwrap();
            left_exp = infix(self, left_exp);
        }

        left_exp
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(Identifier{name:self.cur_token.token_value()})

    }

    fn parse_integer_literal(&mut self) -> Expression {
        let parse = self.cur_token.token_value().parse::<i64>();
        if parse.is_err() {
            self.errors.push(format!(
                "could not parse {} as integer",
                self.cur_token.token_value()
            ));
            return Expression::Error;
        }
        let lit = Expression::IntegerLiteral{value: parse.unwrap()};
        lit
    }

    pub fn parse_boolean(&mut self) -> Expression {
        Expression::Boolean { value:self.cur_toekn_is(Token::TRUE)}
    }

    //All it does is checking whether we have a parsing function associated with p.curToken.Type in the prefix position.
    //If we do, it calls this parsing function, if not, it returns nil.
    //Which it does at the moment, since we haven’t associated any tokens with any parsing functions yet.
    fn parse_prefix_expression(&mut self) -> Expression {
        let cur_token=self.cur_token.clone();
        self.next_token();
        let expr = self.parse_expression(Precedences::PREFIX);
        Expression::Prefix { token: cur_token.clone(), operator: cur_token.token_value(), right: expr.into() }
    }

    fn parse_infix_expression( &mut self, left: Expression) ->  Expression {
        let cur_token = self.cur_token.clone();
        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        Expression::Infix { token: cur_token.clone(), left: left.into(), operator: cur_token.token_value(), right: right.into() }
    }

    fn parse_grouped_expression(&mut self) -> Expression{
        self.next_token();
        let exp = self.parse_expression(Precedences::LOWEST);
        if !self.expect_peek(Token::RPAREN) {
            return Expression::Error;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Expression {
        let cur_token =self.cur_token.clone();
        if !self.expect_peek(Token::LPAREN) {
            return Expression::Error;
        }
        self.next_token();
        let condition = self.parse_expression(Precedences::LOWEST);
        if !self.expect_peek(Token::RPAREN) {
            return Expression::Error;
        }
        if !self.expect_peek(Token::LBRACE) {
            return Expression::Error;
        }
        let consequence = self.parse_block_statement();
        if consequence.is_none(){
            return Expression::Error
        }
        let mut alternative = None;
        if self.peek_token_is(Token::ELSE) {
            self.next_token();
            if !self.expect_peek(Token::LBRACE) {
                return Expression::Error;
            }
            alternative = Some(Rc::new(self.parse_block_statement().unwrap()));
            if alternative.is_none(){
                return Expression::Error
            }
        }

        Expression::If { condition: condition.into(), consequence: consequence.unwrap().into(), alternative: alternative }
    }

    fn parse_functional_literal(&mut self) -> Expression {
        let cur_token=self.cur_token.clone();
        if !self.expect_peek(Token::LPAREN) {
            return Expression::Error;
        }
        let parameters = self.parse_function_parameters();
        if !self.expect_peek(Token::LBRACE) {
            return Expression::Error;
        }
        let body = self.parse_block_statement();
        if body.is_none(){
            return Expression::Error
        }
        Expression::FunctionLiteral { token: cur_token, parameters: parameters, body: body.unwrap().into() }
    }

    fn parse_array_literal(&mut self)->Expression{
        Expression::ArrayLiteral { token: self.cur_token.clone(), elements: self.parse_expression_list(Token::RBRACKET) }
    }
    fn parse_expression_list(&mut self, token:Token)->Vec<Expression>{
        let mut list:Vec<Expression> = vec![];
        if self.peek_token_is(token.clone()){
            self.next_token();
            return list;
        }
        self.next_token();
        list.push(self.parse_expression(Precedences::LOWEST));
        while self.peek_token_is(Token::COMMA){
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedences::LOWEST));
        }
        if !self.expect_peek(token){
            return vec![]
        }
        list
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return identifiers;
        }
        self.next_token();
        let ident = Identifier{name:self.cur_token.token_value()};
        identifiers.push(ident);

        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier{name: self.cur_token.token_value()};
            identifiers.push(ident);
        }

        if !self.expect_peek(Token::RPAREN) {
            return vec![];
        }

        identifiers
    }

    fn parse_call_expression(&mut self,function:Expression)->Expression{
        Expression::Call{
            token:self.cur_token.clone(),
            function: function.into(), 
            arguments:self.parse_expression_list(Token::RPAREN).iter().map(|x|x.clone()).collect()
        }
    }

    fn parse_index_expression(&mut self, left:Expression)->Expression{
        let token=self.cur_token.clone();
        self.next_token();
        let exp = Expression::Index { left:left.into() , index: self.parse_expression(Precedences::LOWEST).into() };
        if !self.expect_peek(Token::RBRACKET){
            return Expression::Error;
        }
        exp
    }
    fn parse_hash_literal(&mut self)->Expression{
        let mut pairs:HashMap<Expression,Expression> = HashMap::new();

        while !self.peek_token_is(Token::RBRACE){
            self.next_token();
            let key = self.parse_expression(Precedences::LOWEST);
            if !self.expect_peek(Token::COLON){
                return Expression::Error;
            }
            self.next_token();
            let value = self.parse_expression(Precedences::LOWEST);
            pairs.insert(key, value);

            if !self.peek_token_is(Token::RBRACE) && !self.expect_peek(Token::COMMA){
                return Expression::Error;
            }
        }
        if !self.expect_peek(Token::RBRACE){
            return Expression::Error
        }
        Expression::HashLiteral { pairs  }
    }

    fn parse_string_literal(&mut self)->Expression{
        Expression::StringLiteral { token: self.cur_token.clone(), value: self.cur_token.token_value() }
    }

    fn cur_toekn_is(&self, t: token::Token) -> bool {
        self.cur_token == t
    }
    fn peek_token_is(&self, t: token::Token) -> bool {
        self.peek_token == t
    }
    ///if the parameter match the peek_token, call next_token methos internally,
    ///which changes the current_token value then set the next peek_token
    fn expect_peek(&mut self, t: token::Token) -> bool {
        //if both expect token and peek token are Token::IDENT type
        if let (Token::IDENT(_x), Token::IDENT(_y)) = (t.clone(), self.peek_token.clone()) {
            self.next_token();
            return true;
        }

        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t.clone());
            false
        }
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: token::Token) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            t.token_value(), self.peek_token.token_type()
        );
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> Precedences {
        self.peek_token.precedence()
    }

    fn current_precedence(&self) -> Precedences {
        self.cur_token.precedence()
    }
    fn no_prefix_parse_fn_error(&mut self, t: Token) {
        self.errors.push(format!(
            "no prefix parse function for {} found",
            t.token_type()
        ));
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::{any::{Any, TypeId}, collections::HashMap, rc::Rc};

    use crate::{
        ast::{Expression,Identifier, Program, Statement},
        lexer::Lexer,
    };

    use super::{ Parser};

    #[test]
    fn test_let_statements(){
        let tests:Vec<(&str,&str,Box<dyn Any>)> = vec![
            ("let x = 5;", "x", Box::new(5)),
            ("let y = true;", "y", Box::new(true)),
            ("let foobar = y;", "foobar", Box::new("y")),
        ];

        for tt in tests{

            let l=Lexer::new(tt.0);
            let mut p= Parser::new(l);
            
            let program:Program= p.parse_program();
            check_parser_errors(&p);
            
            if program.statements.len() != 1{
                panic!("program.Statements does not contain 1 statements. got={}",program.statements.len());
            }

            let stmt = program.statements;
            if !test_let_statement(stmt[0].clone(),tt.1.to_string()){
                return;
            }

            if let Statement::Let { token, ident, value }=stmt[0].clone(){
                if !test_literal_expression(&value, tt.2){
                    return;
                }
            }
        }
    }

    fn test_let_statement(s:Statement,name:String)->bool{

        if let Statement::Let { token, ident, value }=s{
            if token.token_value() != "let"{
                println!("TokenLiteral not 'let'. got={}",token.token_value());
                return false;
            }
            if ident.name != name{
                println!("let statement.name.value not {} got={:?}",name, ident.name);
                return false;
            }
            true
        }else{
            false
        }
    }
    

    #[test]
    fn test_return_statements(){
        let tests:Vec<(&str,Box<dyn Any>)> = vec![
            ("return 5;", Box::new(5)),
            ("return 5-1;", Box::new(4)),
            ("return true;", Box::new(true)),
        ];

        for tt in tests{

            let l=Lexer::new(tt.0);
            let mut p= Parser::new(l);
            
            let program:Program= p.parse_program();
            check_parser_errors(&p);
            
            if program.statements.len() != 1{
                panic!("program.Statements does not contain 1 statements. got={}",program.statements.len());
            }

            let stmt = program.statements;

            if let Statement::Return { token, value }=stmt[0].clone(){
                if !test_literal_expression(&value, tt.1){
                    return;
                }
            }
        }

    }
    
    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        if program.statements.len() != 1 {}
        if let Statement::Expression { token, expression } = program.statements[0].clone(){
            if let Expression::Identifier(ident)=expression{
                if ident.name != "foobar" {
                    println!("ident.value not foobar, got = {}", ident.name);
                }
            }
        }else{
            panic!("program.statements[0] is not ExpresstionStatement.");
        };
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        if program.statements.len() != 1 {}
        if let Statement::Expression { token, expression }=program.statements[0].clone(){
            if let Expression::IntegerLiteral { value } = expression{
                if value != 5 {
                    panic!("value not 5, got = {}", value);
                }
            }
        }else{
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        if program.statements.len() != 1 {}

        if let Statement::Expression { token, expression }= program.statements[0].clone(){
            if let Expression::Boolean { value } = expression{
                if value != true {
                    panic!("Boolean value not {}, got = {}",true, value);
                }
            }else{
                panic!("exp not Boolean. got = None");
            }
        }else{
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
    }
    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_test: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for tt in prefix_test {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.len()
                );
            }
            if let Statement::Expression { token, expression }= program.statements[0].clone(){
                if let Expression::Prefix { token, operator, right }=expression{
                    if operator != tt.1 {
                        panic!("exp.operator is not {}", tt.1);
                    }
                    if !test_integer_literal(right.as_ref(), tt.2) {
                        return;
                    }
                }else{
                    panic!("stmt is not PrefixExpression.");
                }
            }else{
                panic!("program.statement[0] is not ExpresstionStatement.");
            }
        }
    }
    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: Vec<(&str, Box<dyn Any>, &str, Box<dyn Any>)> = vec![
            ("5+5;", Box::new(5_i64), "+", Box::new(5_i64)),
            ("5-5;", Box::new(5_i64), "-", Box::new(5_i64)),
            ("5*5;", Box::new(5_i64), "*", Box::new(5_i64)),
            ("5/5;", Box::new(5_i64), "/", Box::new(5_i64)),
            ("5>5;", Box::new(5_i64), ">", Box::new(5_i64)),
            ("5<5;", Box::new(5_i64), "<", Box::new(5_i64)),
            ("5==5;", Box::new(5_i64), "==", Box::new(5_i64)),
            ("5!=5;", Box::new(5_i64), "!=", Box::new(5_i64)),
            ("true == true", Box::new(true), "==", Box::new(true)),
            ("true != false", Box::new(true), "!=", Box::new(false)),
            ("false == false", Box::new(false), "==", Box::new(false)),
        ];
        for tt in infix_tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.len()
                );
            }
            if let Statement::Expression { token, expression } = program.statements[0].clone(){
                if !test_infix_expression(&expression, tt.1, tt.2, tt.3) {
                    return;
                }
            }else{
                panic!("program.statement[0] is not ExpresstionStatement.");
            }
            
        }
    }
    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ( "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d","((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))","add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)","add((((a + b) + ((c * d) / f)) + g))"),
            ("a * [1, 2, 3, 4][b * c] * d","((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])","add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
        ];
        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.string();
            if actual != tt.1 {
                assert_eq!(tt.1, actual);
                println!("expected={},got={}", tt.1, actual);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x<y) {x}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.len()
            );
        }
        if let Statement::Expression { token, expression } = program.statements[0].clone(){
            if let Expression::If { condition, consequence, alternative }=expression{
                if !test_infix_expression(&condition, Box::new("x"), "<", Box::new("y")) {
                    return;
                }
                
                if let Statement::Block { token, statements }=consequence.as_ref(){
                    if statements.len() !=1{
                        println!( "consequence is not 1 statements. got={}", statements.len());
                    }
                    if let Statement::Expression { token, expression } = statements[0].clone(){
                        if !test_identifier(&expression, "x".to_string()) {
                            return;
                        }
                    }else{
                         panic!("Statement[0] is not ExpressionStatement");
                    }
                }

                if alternative.is_some() {
                    println!("exp.Alternative.statements was not None. got={:?}",alternative.unwrap() )
                }
                
            }else{
                panic!("stmt.expression is not IfExpression");
            }

        }else{
            panic!("program.statement[0] is not ExpresstionStatement.");
        }

        
        
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x<y) {x} else{y}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.len()
            );
        }
        if let Statement::Expression { token, expression }= program.statements[0].clone(){

            if let Expression::If { condition, consequence, alternative }= expression{
                if !test_infix_expression(condition.as_ref(), Box::new("x"), "<", Box::new("y")) {
                    return;
                }

                if let Statement::Block { token, statements } = &*consequence{
                    if statements.len() != 1 {
                        eprintln!("consequence is not 1 statements. got={}",statements.len() );
                    }
                    if let Statement::Expression { token, expression } = statements[0].clone(){
                        if !test_identifier(&expression, "x".to_string()) {
                            return;
                        }
                    }
                }

                if alternative.is_none() {
                    eprintln!("exp.alternative is not statement");
                }

                if let Statement::Block { token, statements } = &*alternative.unwrap(){
                    if let Statement::Expression { token, expression } = &statements[0]{
                        if !test_identifier(&expression, "y".to_string()) {
                            return;
                        }
                    }
                }

            }else{
                panic!("stmet.expression is not IfExpression");
            }
        }else{
            panic!("program.statement[0] is not ExpresstionStatement.");

        }
        
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.len()
            );
        }
        if let Statement::Expression { token, expression } = program.statements[0].clone(){
            if let Expression::FunctionLiteral { token, parameters, body } = expression{
                if parameters.len() != 2 {
                    panic!( "function literal parageters wrong. want 2, got={}",parameters.len());
                }

                let par1 = parameters[0].clone();
                let par2 = parameters[1].clone();
                test_literal_expression(&Expression::Identifier(par1), Box::new("x"));
                test_literal_expression(&Expression::Identifier(par2), Box::new("y"));

                if let Statement::Block { token, statements } = &*body{
                    if statements.len()!=1{
                        panic!("function.Body.Statements has not 1 statements. got={}", statements.len());
                    }
                    if let Statement::Expression { token, expression } = statements[0].clone(){
                        test_infix_expression(&expression,Box::new("x"),"+",Box::new("y"),);
                    }else{
                        panic!("function body stmt is not ast.ExpressionStatement.");
                    }
                }else{
                    
                }

            }else{
                panic!("stmet.expression is not IfExpression");
            }
        }else{
            panic!("program.statement[0] is not ExpresstionStatement.");
        }

        
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests: Vec<(&str, Vec<&str>)> = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.len()
                );
            }
            if let Statement::Expression { token, expression }= program.statements[0].clone(){
                if let Expression::FunctionLiteral { token, parameters, body }= expression{
                    if parameters.len() != tt.1.len(){
                        eprintln!("length parameters worng. want {}, got={}",tt.1.len(),parameters.len());
                    }
                    
                    for (i,ident) in tt.1.into_iter().enumerate(){
                        test_literal_expression(&&Expression::Identifier(parameters[i].clone()), Box::new(ident));
                    }
                }else{

                }
            }else{
                panic!("program.statement[0] is not ExpresstionStatement.");
            }
            
        }
    }

    #[test]
    fn test_call_expression_parsing(){
        let input="add(1, 2 * 3, 4 + 5);";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.len()
            );
        }
        if let Statement::Expression { token, expression }= program.statements[0].clone(){
            if let Expression::Call { token, function, arguments }=expression{
                if !test_identifier(&function, "add".to_string()){
                    return
                }
                if arguments.len() != 3{
                    panic!("wrong length of arguments. got={}",arguments.len());
                }
                test_literal_expression(&arguments[0], Box::new(1));
                test_infix_expression(&arguments[1], Box::new(2),"*",Box::new(3));
                test_infix_expression(&arguments[2], Box::new(4),"+",Box::new(5));
            }else{
                panic!("stmt.expression is not CAllExpression");
            }
        }else{
            panic!("program.statement[0] is not ExpresstionStatement.");
        }
        
    }

    #[test]
    fn test_string_literal_expression(){
        let input = r#""hello world";"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression }= program.statements[0].clone(){
            if let Expression::StringLiteral { token, value }=expression{
                if value !="hello world"{
                    println!("literal value not hello world. got={}",value);
                }
            }else{
                panic!("exp not StringLiteral. got={:?}",expression);
            }
        }else{
            panic!("program.statement[0] is not ExpresstionStatement.");
        }
    }

    #[test]
    fn test_parsing_array_literal(){
        let input = "[1,2*2,3+3]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression } =&program.statements[0]{
            if let Expression::ArrayLiteral { token, elements } = expression{
                if elements.len() !=3{
                    panic!("elements len() not 3. got={}",elements.len());
                }
                test_integer_literal(&elements[0], 1);
                test_infix_expression(&elements[1], Box::new(2_i64), "*", Box::new(2_i64));
                test_infix_expression(&elements[2], Box::new(3_i64), "+", Box::new(3_i64));
            }else{
                panic!("exp not ArrayLiterl");
            }
        }
    }

    #[test]
    fn test_parsing_index_expression(){
        let input = "myArray[1 + 1]";
        let l = Lexer::new(input);
        let mut p= Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression } = &program.statements[0]{
            if let Expression::Index { left, index }= expression{
                if !test_identifier(&left, "myArray".to_string()){
                    return;
                }
                if !test_infix_expression(&index, Box::new(1), "+", Box::new(1)){
                    return;
                }
            }else{
                panic!("exp not IndexExpression.");
            }
        }
    }
    #[test]
    fn test_parsing_hash_literals_string_keys(){
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression }=&program.statements[0]{
            if let Expression::HashLiteral { pairs }=expression{
                if pairs.len() != 3{
                    panic!("hash.pairs has wrong length. got={}",pairs.len());
                }
                let expected:HashMap<&str,i64> = HashMap::from([
                    ("one",1),
                    ("two",2),
                    ("three",3),
                ]);
                for (key,value) in pairs{
                    let v=value;
                    if let Expression::StringLiteral { token, value } = key{
                        let expected_value = expected.get(value.as_str());
                        test_integer_literal(v, *expected_value.unwrap());
                    }else{
                        panic!("key is not expression.stringliteral");
                    }
                }
            }else{
                panic!("exp is not HashLiteral. got={:?}",expression);
            }
        }
    }

    #[test]
    fn test_parseing_empty_hash_literal(){
        let input = "{}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression }=&program.statements[0]{
            if let Expression::HashLiteral { pairs }=expression{
                if pairs.len() !=0{
                    println!("hash.pairs has wrong length. got={}",pairs.len());
                }
            }else{
                panic!("exp is not HashLiteral. got={:?}",expression);
            }

        }

    }
    #[test]
    fn test_parsing_hash_literals_with_expressions(){
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if let Statement::Expression { token, expression }=&program.statements[0]{
            if let Expression::HashLiteral { pairs }=expression{
                if pairs.len() !=3{
                    println!("hash.pairs has wrong length. got={}",pairs.len());
                }
                for (k,v) in pairs{
                    if let Expression::StringLiteral { token, value } = k{
                        match value.as_str(){
                            "one"=>{
                                test_infix_expression(v, Box::new(0_i64), "+", Box::new(1_i64));
                            },
                            "two"=>{
                                test_infix_expression(v, Box::new(0_i64), "+", Box::new(1_i64));
                            },
                            "three"=>{
                                test_infix_expression(v, Box::new(0_i64), "+", Box::new(1_i64));
                            },
                            _=>{
                                println!("No test function for kye {} found",value);
                            }
                        }
                    }
                }
            }else{
                panic!("exp is not HashLiteral. got={:?}",expression);
            }

        }
    }


    fn test_infix_expression(exp: &Expression, left1: Box<dyn Any>, operator1: &str, right1: Box<dyn Any>) -> bool {

        if let Expression::Infix { token, left, operator, right }=exp{
            if !test_literal_expression(left.as_ref(), left1) {
                return false;
            }
            if operator != operator1 {
                return false;
            }
            if !test_literal_expression(right.as_ref(), right1) {
                return false;
            }
            true

        }else{
            false
        }

    }

    fn test_literal_expression(exp: &Expression, expected: Box<dyn Any>) -> bool {
        // match exp{
        //     Expression::Boolean { token, value }=>test_boolean_literal(),
        //     Expression::IntegerLiteral { value }=>test_integer_literal(),
        //     Expression::Identifier(ident)=>test_identifier(),
        //     _=>false
        // }
        if (&*expected).type_id() == TypeId::of::<usize>() {
            test_integer_literal(exp, *expected.downcast::<usize>().unwrap() as i64)
        } else if (&*expected).type_id() == TypeId::of::<i32>() {
            test_integer_literal(exp, *expected.downcast::<i32>().unwrap() as i64)
        } else if (&*expected).type_id() == TypeId::of::<i64>() {
            test_integer_literal(exp, *expected.downcast::<i64>().unwrap())
        } else if (&*expected).type_id() == TypeId::of::<String>() {
            test_identifier(exp, *expected.downcast::<String>().unwrap())
        } else if (&*expected).type_id() == TypeId::of::<&str>() {
            test_identifier(exp, (*expected.downcast::<&str>().unwrap()).to_string())
        } else if (&*expected).type_id() == TypeId::of::<bool>() {
            test_boolean_literal(exp, *expected.downcast::<bool>().unwrap())
        } else {
            false
        }
    }

    fn test_identifier(exp: &Expression, value: String) -> bool {
        let ident = if let Expression::Identifier(id)=exp{
            Some(id)
        }else{
            None
        };
        if ident.is_none() {
            println!("exp not Identifier.");
            return false;
        }
        let ident = ident.unwrap();
        if ident.name != value {
            println!( "ident.token_literal() not {}. got={}", value, ident.name);
            return false;
        }
        true
    }

    fn test_integer_literal(il: &Expression, value: i64) -> bool {
        let integ = if let Expression::IntegerLiteral { value }=il{
            Some(value)
        }else{
            None
        };
        if integ.is_none() {
            println!("il not IntegerLiteral");
            return false;
        }
        let integ = integ.unwrap();
        if *integ != value {
            println!("integ.value not {}. got ={}", value, integ);
            return false;
        }
        true
    }

    fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
        let bo = if let Expression::Boolean {  value }=exp{
            Some(value)
        }else{
            None
        };
        if bo.is_none() {
            println!("exp no Boolean ");
            return false;
        }
        let bo = bo.unwrap();
        if *bo != value {
            println!("bo.boolean_value not {}. got={}", value, bo);
            return false;
        }
        true
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());
        for msg in errors {
            eprintln!("parser error: {}", msg);
        }
    }
}
