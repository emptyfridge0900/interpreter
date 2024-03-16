use std::collections::HashMap;

use crate::{
    ast::{
        self, BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement
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
}

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(&mut Parser, ex: Option<Box<dyn Expression>>) -> Option<Box<dyn Expression>>;
pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
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

        p.register_prefix(Token::TRUE.token_type(), Parser::parse_boolean);
        p.register_prefix(Token::FALSE.token_type(), Parser::parse_boolean);

        p.register_prefix(Token::LPAREN.token_type(), Parser::parse_grouped_expression);

        p.register_prefix(Token::IF.token_type(), Parser::parse_if_expression);

        p.register_prefix(Token::FUNCTION.token_type(),Parser::parse_functional_literal);

        p.register_infix(Token::LPAREN.token_type(), Parser::parse_call_expression);
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
        let program = ast::Program::new();
        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                program.statements.borrow_mut().push(stmt.unwrap());
            }
            self.next_token()
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match &self.cur_token {
            Token::LET => {
                let stmt = self.parse_let_statement();
                if stmt.is_none() {
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            }
            Token::RETURN => {
                let stmt = self.parse_return_statement();
                if stmt.is_none() {
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            }
            _ => {
                let stmt = self.parse_expression_statement();
                if stmt.is_none() {
                    return None;
                }
                Some(Box::new(stmt.unwrap()))
            }
        }
    }
    //parseLetStatement 예제와 순서가 좀 다른게 진행... 나중에 에러날수도 있음
    //원래 순서는 LetStatement를 먼저 token과 만들고,나중에 name을 추가해주는 방식인데
    //여기서는 먼저 name을 만들고 나중에 LetStatement를 만든다
    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let let_token = self.cur_token.clone();
        if !self.expect_peek(Token::IDENT("any letter for now".to_string())) {
            return None;
        }

        let identifier = match self.cur_token {
            Token::IDENT(ref s) => Identifier {
                token_type: self.cur_token.token_type(),
                token_value: s.clone(),
            },
            _ => {
                return None;
            }
        };

        //let name = ast::Identifier::new(self.cur_token.to_owned(),self.cur_token.to_string());
        let stmt = ast::LetStatement::new(let_token, identifier);

        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }

        //todo
        while !self.cur_toekn_is(Token::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }
    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let save_token = self.cur_token.clone();
        let stmt = ReturnStatement::new(save_token);
        self.next_token();

        //todo
        while !self.cur_toekn_is(Token::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }
    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let exp = self.parse_expression(Precedences::LOWEST);
        let stmt = ExpressionStatement {
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

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block = BlockStatement::new(self.cur_token.clone());
        self.next_token();
        while !self.cur_toekn_is(Token::RBRACE) && !self.cur_toekn_is(Token::EOF) {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                block.statements.push(stmt.unwrap());
            }
            self.next_token();
        }

        Some(block)
    }

    fn parse_expression(&mut self, precedence: Precedences) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type());
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.cur_token.clone());
            return None;
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

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier::new(
            self.cur_token.token_type(),
            self.cur_token.token_value(),
        )))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let parse = self.cur_token.token_value().parse::<i64>();
        if parse.is_err() {
            self.errors.push(format!(
                "could not parse {} as integer",
                self.cur_token.token_value()
            ));
            return None;
        }
        let lit = IntegerLiteral::new(self.cur_token.clone(), parse.unwrap());

        Some(Box::new(lit))
    }
    pub fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Boolean::new(
            self.cur_token.clone(),
            self.cur_toekn_is(Token::TRUE),
        )))
    }

    //All it does is checking whether we have a parsing function associated with p.curToken.Type in the prefix position.
    //If we do, it calls this parsing function, if not, it returns nil.
    //Which it does at the moment, since we haven’t associated any tokens with any parsing functions yet.
    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression =
            PrefixExpression::new(self.cur_token.clone(), self.cur_token.token_value(), None);
        self.next_token();
        let expr = self.parse_expression(Precedences::PREFIX);
        expression.right = expr;
        Some(Box::new(expression))
    }
    fn parse_infix_expression(
        &mut self,
        left: Option<Box<dyn Expression>>,
    ) -> Option<Box<dyn Expression>> {
        let mut expression = InfixExpression::new(
            self.cur_token.clone(),
            left,
            self.cur_token.token_value(),
            None,
        );
        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        expression.right = right;
        Some(Box::new(expression))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();
        let exp = self.parse_expression(Precedences::LOWEST);
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression = IfExpression::new(self.cur_token.clone(), None, None, None);
        if !self.expect_peek(Token::LPAREN) {
            return None;
        }
        self.next_token();
        expression.condition = self.parse_expression(Precedences::LOWEST);
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        if !self.expect_peek(Token::LBRACE) {
            return None;
        }
        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(Token::ELSE) {
            self.next_token();
            if !self.expect_peek(Token::LBRACE) {
                return None;
            }
            expression.alternative = self.parse_block_statement();
        }

        Some(Box::new(expression))
    }

    fn parse_functional_literal(&mut self) -> Option<Box<dyn Expression>> {
        let mut lit = FunctionLiteral::new(self.cur_token.clone(), vec![], None);
        if !self.expect_peek(Token::LPAREN) {
            return None;
        }
        lit.parameters = self.parse_function_parameters();
        if !self.expect_peek(Token::LBRACE) {
            return None;
        }
        lit.body = self.parse_block_statement();
        Some(Box::new(lit))
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return identifiers;
        }
        self.next_token();
        let ident = Identifier::new(self.cur_token.token_type(), self.cur_token.token_value());
        identifiers.push(ident);

        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier::new(self.cur_token.token_type(), self.cur_token.token_value());
            identifiers.push(ident);
        }

        if !self.expect_peek(Token::RPAREN) {
            return vec![];
        }

        identifiers
    }

    fn parse_call_expression(&mut self,function:Option<Box<dyn Expression>>)->Option<Box<dyn Expression>>{
        Some(Box::new(CallExpression::new(self.cur_token.clone(),function.unwrap(), self.parse_call_arguments())))
    }
    fn parse_call_arguments(&mut self)->Vec<Box<dyn Expression>>{
        let mut args= vec![];
        if self.peek_token_is(Token::RPAREN){
            self.next_token();
            return args;
        }
        self.next_token();
        args.push(self.parse_expression(Precedences::LOWEST).unwrap());
        while self.peek_token_is(Token::COMMA){
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedences::LOWEST).unwrap());
        }
        if !self.expect_peek(Token::RPAREN){
            return vec![];
        }

        args
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
        if let (Token::IDENT(x), Token::IDENT(y)) = (t.clone(), self.peek_token.clone()) {
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
            t.token_value(), self.peek_token.token_value()
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
    use std::{
        any::{Any, TypeId},
        borrow::Borrow,
    };

    use crate::{
        ast::{
            Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, Node, PrefixExpression, Program
        },
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.borrow().len(), 1);
        if program.statements.borrow().len() != 1 {}
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
        let stmt = stmt.unwrap();
        let test = stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<Identifier>();

        if test.is_none() {}
        let ident = test.unwrap();
        if ident.token_value != "foobar" {
            println!("ident.value not foobar, got = {}", ident.token_literal());
        }
        if ident.token_literal() != "foobar" {
            println!(
                "ident.token_literal() not foobar, got ={}",
                ident.token_literal()
            );
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.borrow().len(), 1);
        if program.statements.borrow().len() != 1 {}
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
        let stmt = stmt.unwrap();
        let wrapped = stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>();

        if wrapped.is_none() {
            panic!("exp not IntegerLiteral. got = None");
        }
        let literal = wrapped.unwrap();

        if literal.integer_value != 5 {
            panic!("literal value not 5, got = {}", literal.integer_value);
        }
        if literal.token_literal() != "5" {
            panic!(
                "literal.token_literal not \"5\" got = {}",
                literal.token_literal()
            );
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.borrow().len(), 1);
        if program.statements.borrow().len() != 1 {}

        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statements[0] is not ExpresstionStatement.");
        }
        let stmt = stmt.unwrap();
        let boolean = stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<Boolean>();

        if boolean.is_none() {
            panic!("exp not Boolean. got = None");
        }
        let boolean = boolean.unwrap();
        if boolean.boolean_value != true {
            panic!(
                "Boolean value not {}, got = {}",
                true, boolean.boolean_value
            );
        }
        if boolean.token_literal() != "true" {
            panic!(
                "Boolean.token_literal not \"true\" got = {}",
                boolean.token_literal()
            );
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

            if program.statements.borrow().len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.borrow().len()
                );
            }
            let binding = program.statements.borrow();
            let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
            if stmt.is_none() {
                panic!("program.statement[0] is not ExpresstionStatement.");
            }

            let exp = stmt
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<PrefixExpression>();

            if exp.is_none() {
                panic!("stmt is not PrefixExpression.");
            }
            if exp.unwrap().operator != tt.1 {
                panic!("exp.operator is not {}", tt.1);
            }
            let exp = exp.unwrap();
            if !test_integer_literal(exp.right.as_ref(), tt.2) {
                return;
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

            if program.statements.borrow().len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.borrow().len()
                );
            }
            let binding = program.statements.borrow();
            let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
            if stmt.is_none() {
                panic!("program.statement[0] is not ExpresstionStatement.");
            }
            if !test_infix_expression(stmt.unwrap().expression.as_ref(), tt.1, tt.2, tt.3) {
                return;
            }

            // let exp = stmt.unwrap()
            // .expression.as_ref().unwrap().as_any().downcast_ref::<InfixExpression>();

            // assert_eq!(exp.is_some(),true);
            // if exp.is_none(){
            //     println!("stmt is not InfixExpression.");
            // }
            // let exp=exp.unwrap();
            // if !test_integer_literal(exp.left.as_ref(), tt.1){
            //     return
            // }
            // if exp.operator != tt.2{
            //     println!("exp.operator is not {} got={}",tt.2,exp.operator);
            // }
            // if !test_integer_literal(exp.right.as_ref(), tt.3){
            //     return
            // }
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
            ("add(a + b + c * d / f + g)","add((((a + b) + ((c * d) / f)) + g))")
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
        if program.statements.borrow().len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.borrow().len()
            );
        }
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statement[0] is not ExpresstionStatement.");
        }
        let exp = stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IfExpression>();
        if exp.is_none() {
            panic!("stmt.expression is not IfExpression");
        }
        let exp = exp.unwrap();
        if !test_infix_expression(exp.condition.as_ref(), Box::new("x"), "<", Box::new("y")) {
            return;
        }
        if exp.consequence.is_none() {}
        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            println!(
                "consequence is not 1 statements. got={}",
                exp.consequence.as_ref().unwrap().statements.len()
            );
        }
        let consequence = exp.consequence.as_ref().unwrap().statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        if consequence.is_none() {
            panic!("Statement[0] is not ExpressionStatement");
        }
        let consequence = consequence.unwrap();
        if !test_identifier(consequence.expression.as_ref(), "x".to_string()) {
            return;
        }
        if exp.alternative.is_some() {
            println!(
                "exp.Alternative.statements was not None. got={}",
                exp.alternative.as_ref().unwrap().string()
            )
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x<y) {x} else{y}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.borrow().len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.borrow().len()
            );
        }
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statement[0] is not ExpresstionStatement.");
        }
        let exp = stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IfExpression>();
        if exp.is_none() {
            panic!("stmet.expression is not IfExpression");
        }
        let exp = exp.unwrap();
        if !test_infix_expression(exp.condition.as_ref(), Box::new("x"), "<", Box::new("y")) {
            return;
        }
        if exp.consequence.is_none() {}
        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            eprintln!(
                "consequence is not 1 statements. got={}",
                exp.consequence.as_ref().unwrap().statements.len()
            );
        }
        let consequence = exp.consequence.as_ref().unwrap().statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        if consequence.is_none() {
            panic!("Statement[0] is not ExpressionStatement");
        }
        let consequence = consequence.unwrap();
        if !test_identifier(consequence.expression.as_ref(), "x".to_string()) {
            return;
        }

        if exp.alternative.is_none() {
            eprintln!("exp.alternative is not statement");
        }

        let alternative = exp.alternative.as_ref().unwrap().statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        if alternative.is_none() {
            panic!("Statement[0] is not ExpressionStatement");
        }
        let alternative = alternative.unwrap();
        if !test_identifier(alternative.expression.as_ref(), "y".to_string()) {
            return;
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if program.statements.borrow().len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.borrow().len()
            );
        }
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statement[0] is not ExpresstionStatement.");
        }

        let function = stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<FunctionLiteral>();
        if function.is_none() {
            panic!("stmet.expression is not IfExpression");
        }
        let function = function.unwrap();
        if function.parameters.len() != 2 {
            panic!(
                "function literal parageters wrong. want 2, got={}",
                function.parameters.len()
            );
        }
        let par1: Box<dyn Expression> = Box::new(function.parameters[0].clone());
        let par2: Box<dyn Expression> = Box::new(function.parameters[1].clone());
        test_literal_expression(Some(&par1), Box::new("x"));
        test_literal_expression(Some(&par2), Box::new("y"));

        if function.body.is_none() {

        }
        if function.body.as_ref().unwrap().statements.len() !=1{
            panic!("function.Body.Statements has not 1 statements. got={}",function.body.as_ref().unwrap().statements.len());
        }
        let body_stmt = function.body.as_ref().unwrap().statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>();
        if body_stmt.is_none() {
            panic!("function body stmt is not ast.ExpressionStatement.");
        }

        test_infix_expression(
            body_stmt.unwrap().expression.as_ref(),
            Box::new("x"),
            "+",
            Box::new("y"),
        );
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

            if program.statements.borrow().len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got = {}",
                    1,
                    program.statements.borrow().len()
                );
            }
            let binding = program.statements.borrow();
            let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
            if stmt.is_none() {
                panic!("program.statement[0] is not ExpresstionStatement.");
            }
            let stmt = stmt.unwrap();
            let function = stmt.expression.as_ref().unwrap().as_any().downcast_ref::<FunctionLiteral>();
            if function.is_none(){

            }

            if function.unwrap().parameters.len() != tt.1.len(){
                eprintln!("length parameters worng. want {}, got={}",tt.1.len(),function.unwrap().parameters.len());
            }
            for (i,ident) in tt.1.into_iter().enumerate(){
                let param:Box<dyn Expression> = Box::new(function.unwrap().parameters[i].clone());
                test_literal_expression(Some(&param), Box::new(ident));
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
        if program.statements.borrow().len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got = {}",
                1,
                program.statements.borrow().len()
            );
        }
        let binding = program.statements.borrow();
        let stmt = binding[0].as_any().downcast_ref::<ExpressionStatement>();
        if stmt.is_none() {
            panic!("program.statement[0] is not ExpresstionStatement.");
        }
        let exp = &stmt.unwrap().expression;
        let exp= exp.as_ref().unwrap().as_any().downcast_ref::<CallExpression>();
        if exp.is_none(){
            panic!("stmt.expression is not CAllExpression");
        }
        let exp=exp.unwrap();
        if !test_identifier(Some(&exp.function), "add".to_string()){
            return
        }
        if exp.arguments.len() != 3{
            panic!("wrong length of arguments. got={}",exp.arguments.len());
        }
        test_literal_expression(Some(&exp.arguments[0]), Box::new(1));
        test_infix_expression(Some(&exp.arguments[1]), Box::new(2),"*",Box::new(3));
        test_infix_expression(Some(&exp.arguments[2]), Box::new(4),"+",Box::new(5));
    }


    fn test_infix_expression(
        exp: Option<&Box<dyn Expression>>,
        left: Box<dyn Any>,
        operator: &str,
        right: Box<dyn Any>,
    ) -> bool {
        let op_exp = exp.unwrap().as_any().downcast_ref::<InfixExpression>();
        if op_exp.is_none() {
            return false;
        }
        let op_exp = op_exp.unwrap();
        if !test_literal_expression(op_exp.left.as_ref(), left) {
            return false;
        }
        if op_exp.operator != operator {
            return false;
        }
        if !test_literal_expression(op_exp.right.as_ref(), right) {
            return false;
        }
        true
    }

    fn test_literal_expression(exp: Option<&Box<dyn Expression>>, expected: Box<dyn Any>) -> bool {
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

    fn test_identifier(exp: Option<&Box<dyn Expression>>, value: String) -> bool {
        let ident = exp.as_ref().unwrap().as_any().downcast_ref::<Identifier>();
        if ident.is_none() {
            println!("exp not Identifier.");
            return false;
        }
        let ident = ident.unwrap();
        if ident.token_value != value {
            println!("ident.token_value not {}. got={}", value, ident.token_value);
            return false;
        }
        if ident.token_literal() != value {
            println!(
                "ident.token_literal() not {}. got={}",
                value,
                ident.token_literal()
            );
            return false;
        }
        true
    }

    fn test_integer_literal(il: Option<&Box<dyn Expression>>, value: i64) -> bool {
        let integ = il.unwrap().as_any().downcast_ref::<IntegerLiteral>();
        if integ.is_none() {
            println!("il not IntegerLiteral");
            return false;
        }
        let integ = integ.unwrap();
        if integ.integer_value != value {
            println!("integ.value not {}. got ={}", value, integ.integer_value);
            return false;
        }
        if integ.token_literal() != format!("{}", value) {
            println!(
                "integ.token_literal() not {}. got={}",
                value,
                integ.token_literal()
            );
            return false;
        }
        true
    }

    fn test_boolean_literal(exp: Option<&Box<dyn Expression>>, value: bool) -> bool {
        let bo = exp.unwrap().as_any().downcast_ref::<Boolean>();
        if bo.is_none() {
            println!("exp no Boolean ");
            return false;
        }
        let bo = bo.unwrap();
        if bo.boolean_value != value {
            println!("bo.boolean_value not {}. got={}", value, bo.boolean_value);
            return false;
        }
        if bo.token_literal() != value.to_string() {
            println!("bo.token_literal not {}. got={}", value, bo.token_literal());
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
