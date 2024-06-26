use std::any::{Any, TypeId};

use crate::{
    ast::{BlockStatement, Boolean, ExpressionStatement, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement}, environment::Environment, object::Object
};

const TRUE: Object= Object::Boolean(true);
const FALSE: Object= Object::Boolean(false);
const NULL:Object = Object::Null;

pub fn eval(node: &dyn Any,env:&mut Environment) -> Object {
    let ret = 
    if (&*node).type_id() == TypeId::of::<Program>(){
        let program = node.downcast_ref::<Program>();
        eval_program(program.unwrap(),env)
    } else if (&*node).type_id() == TypeId::of::<ExpressionStatement>(){
        let exp_stmt = node.downcast_ref::<ExpressionStatement>();
        eval(exp_stmt.unwrap().expression.as_ref().unwrap().as_any(),env)

    } else if (&*node).type_id() == TypeId::of::<IntegerLiteral>() {
        let integer_literal = node.downcast_ref::<IntegerLiteral>();
        let ret = integer_literal.unwrap().integer_value;
        Object::Integer(ret)

    } else if (&*node).type_id() == TypeId::of::<Boolean>() {
        let boolean = node.downcast_ref::<Boolean>();
        let ret = boolean.unwrap().boolean_value;
        native_boolean_to_boolean_object(ret)

    } else if (&*node).type_id() == TypeId::of::<PrefixExpression>() {
        let prefix_exp = node.downcast_ref::<PrefixExpression>();
        let x =prefix_exp.unwrap();
        let right = eval(x.right.as_ref().unwrap().as_any(),env);
        if is_error(right.clone()){
            return right;
        }
        eval_prefix_expression(&prefix_exp.unwrap().operator,right)

    } else if (&*node).type_id() == TypeId::of::<InfixExpression>(){
        let infix_exp = node.downcast_ref::<InfixExpression>();
        let x =infix_exp.unwrap();
        let left = eval(x.left.as_ref().unwrap().as_any(),env);
        if is_error(left.clone()){
            return left;
        }
        let right = eval(x.right.as_ref().unwrap().as_any(),env);
        if is_error(right.clone()){
            return right;
        }
        eval_infix_expression(&infix_exp.unwrap().operator,left,right)

    } else if (&*node).type_id() == TypeId::of::<BlockStatement>(){
        let statement = node.downcast_ref::<BlockStatement>();
        let x = statement.unwrap();
        //eval_statements(x.statements.iter().map(|y|y).collect())
        
        eval_block_statement(x,env)
    } else if (&*node).type_id() == TypeId::of::<IfExpression>(){
        let if_exp = node.downcast_ref::<IfExpression>();
        eval_if_expression(if_exp.unwrap(),env)

    } else if (&*node).type_id() == TypeId::of::<ReturnStatement>(){
        let ret_statement = node.downcast_ref::<ReturnStatement>();
        let val = eval(ret_statement.unwrap().return_value.as_ref().unwrap().as_any(),env);
        if is_error(val.clone()){
            return val;
        }
        Object::Return(Box::new(val))

    } else if (&*node).type_id() == TypeId::of::<LetStatement>(){
        let let_statement = node.downcast_ref::<LetStatement>();
        let let_statement = let_statement.unwrap();
        let val = eval(let_statement.value.as_ref().unwrap().as_any(),env);
        if is_error(val.clone()){
            return val;
        }
        env.set(let_statement.name.token_value.clone(), val)

    } else if (&*node).type_id() == TypeId::of::<Identifier>(){
        let identifier = node.downcast_ref::<Identifier>();
        let identifier = identifier.unwrap();
        eval_identifier(identifier, env)
    } else {
        Object::Unknown
    };

    ret
}

fn eval_program(program:&Program,env:&mut Environment)->Object{
    let binding = program.statements.borrow();
    let stmt:Vec<&Box<dyn Statement>> = binding.iter().map(|x|x).collect();
    let mut result:Object = Object::Null;
    for statement in stmt{
        result = eval(statement.as_any(),env);
        // if let Object::Return(value) = result{
        //     return *value;
        // }
        match result{
            Object::Return(value)=>return *value,
            Object::Error(message)=>return Object::Error(message),
            _=>continue
        }
    }
    result
}
fn eval_block_statement(block:&BlockStatement,env:&mut Environment)->Object{
    let mut result:Object = Object::Null;
    for statement in &block.statements{
        result = eval(statement.as_any(),env);

        match result{
            Object::Return(val)=>return Object::Return(val),
            Object::Error(message)=>return Object::Error(message),
            _=>continue
        }
    }
    result
}

fn native_boolean_to_boolean_object(input:bool)->Object{
    if input{
        return TRUE;
    }
    FALSE
}
fn eval_prefix_expression(operator:&str, right:Object)-> Object{
    match operator{
        "!"=>eval_bang_operator_expression(right),
        "-"=>eval_minus_prefix_operator_expression(right),
        _=>new_error(format!("unknown operator: {} {}",operator,right.get_type()))
    }
}
fn eval_infix_expression(operator:&str,left:Object,right:Object)->Object{
    if operator == "=="{
        native_boolean_to_boolean_object(left==right)
    } else if operator == "!="{
        native_boolean_to_boolean_object(left != right)
    } else if let (Object::Integer(l), Object::Integer(r))=(left.clone(),right.clone()){
        eval_integer_infix_expression(operator,l,r)
    } else if left.get_type() != right.get_type(){
        new_error(format!("type mismatch: {} {} {}",left.get_type(),operator,right.get_type()))
    } else {
        new_error(format!("unknown operator: {} {} {}",left.get_type(),operator,right.get_type()))
    }
    // match (left,right){
    //     (Object::Integer(l),Object::Integer(r))=>eval_integer_infix_expression(operator,l,r),
    //     _=>NULL
    // }
}
fn eval_identifier(node:&Identifier,env:&Environment)->Object{
    let val = env.get(node.token_value.clone());
    if val.is_none(){
        println!("identifier not found: {}",node.token_value.clone());
    }
    val.unwrap()
}
fn eval_bang_operator_expression(right:Object)->Object{
    match right{
        TRUE=>FALSE,
        FALSE=>TRUE,
        NULL=>TRUE,
        _=>FALSE
        
    }
}
fn eval_minus_prefix_operator_expression(right:Object)->Object{
    match right{
        Object::Integer(i)=>Object::Integer(-i),
        _=>new_error(format!("unknown operator: -{}",right.get_type()))
    } 
}
fn eval_integer_infix_expression(operator:&str,left:i64,right:i64)->Object{

    match operator{
        "+"=>Object::Integer(left+right),
        "-"=>Object::Integer(left-right),
        "*"=>Object::Integer(left*right),
        "/"=>Object::Integer(left/right),
        "<"=>native_boolean_to_boolean_object(left<right),
        ">"=>native_boolean_to_boolean_object(left>right),
        "=="=>native_boolean_to_boolean_object(left==right),
        "!="=>native_boolean_to_boolean_object(left!=right),
        _=>new_error(format!("unknown operator: {} {} {}",left, operator, right))
    }
}
fn eval_if_expression(ie:&IfExpression,env:&mut Environment)->Object{
    let condition = eval(ie.condition.as_ref().unwrap().as_any(),env);
    if is_error(condition.clone()){
        return condition;
    }
    if is_truthy(condition){
        eval(ie.consequence.as_ref().unwrap().as_any(),env)
    } else if ie.alternative.is_some(){
        eval(ie.alternative.as_ref().unwrap().as_any(),env)
    } else {
        NULL
    }
}
fn is_truthy(obj:Object)->bool{
    match obj{
        NULL=>false,
        TRUE=>true,
        FALSE=>false,
        _=>true
    }
}
fn new_error(format:String)->Object{
    Object::Error(format)
}
fn is_error(obj:Object)->bool{
    if obj.get_type()=="ERROR"{
        return true;
    }
    false
}
#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::{ast::Node, environment::Environment, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![
            ("5", 5), 
            ("10", 10),
            ("-5",-5),
            ("-10",-10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
        ];
        for tt in tests {
            let evaluated = test_eval(tt.0);
            test_integer_object(evaluated, tt.1);
        }
    }
    #[test]
    fn test_eval_boolean_expression(){
        let tests:Vec<(&str,bool)> = vec![
            ("true",true),
            ("false",false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for tt in tests {
            let evaluated = test_eval(tt.0);
            test_boolean_object(evaluated, tt.1);
        }
    }
    #[test]
    fn test_bang_operator(){
        let tests:Vec<(&str,bool)> =vec![("!true",false),("!false",true),("!5",false),("!!true",true),("!!false",false),("!!5",true)];
        for tt in tests{
            let evaluated = test_eval(tt.0);
            test_boolean_object(evaluated, tt.1);
        }
    }
    #[test]
    fn test_if_else_expression(){
        let tests : Vec<(&str,Box<dyn Any>)> = vec![
            ("if (true) { 10 }", Box::new(10_i64)),
            ("if (false) { 10 }", Box::new(Object::Null)),
            ("if (1) { 10 }", Box::new(10_i64)),
            ("if (1 < 2) { 10 }", Box::new(10_i64)),
            ("if (1 > 2) { 10 }", Box::new(Object::Null)),
            ("if (1 > 2) { 10 } else { 20 }", Box::new(20_i64)),
            ("if (1 < 2) { 10 } else { 20 }", Box::new(10_i64)),
        ];

        for tt in tests{
            let evaluated = test_eval(tt.0);
            let integer = tt.1.downcast::<i64>();
            if integer.is_err(){
                test_null_object(evaluated);
            }else{
                test_integer_object(evaluated, *integer.unwrap());
            }
        }
    }
    #[test]
    fn test_return_statements(){
        let tests:Vec<(&str,i64)> = vec![
            ("return 10;", 10_i64),
            ("return 10; 9;", 10_i64),
            ("return 2 * 5; 9;", 10_i64),
            ("9; return 2 * 5; 9;", 10_i64),
        ];
        for tt in tests{
            let evaluated = test_eval(tt.0);
            test_integer_object(evaluated, tt.1);
        }
    }
    #[test]
    fn test_error_handling(){
        let tests:Vec<(&str,&str)> = vec![
            (
            "5 + true;",
            "type mismatch: INTEGER + BOOLEAN",
            ),
            (
            "5 + true; 5;",
            "type mismatch: INTEGER + BOOLEAN",
            ),
            (
            "-true",
            "unknown operator: -BOOLEAN",
            ),
            (
            "true + false;",
            "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
            "5; true + false; 5",
            "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
"
if (10 > 1) {
if (10 > 1) {
return true + false;
}
return 1;
}
",
"unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "foobar",
                "identifier not found: foobar",
            )
        ];
        for tt in tests{
            let evaluated =  test_eval(tt.0);
            if let Object::Error(message) = evaluated.clone(){
                if message!=tt.1{
                    println!("wrong error message. expected={}, got={}",tt.1,message);
                }else{
                    continue;
                }
            }
            println!("no error object returned. got={:?}",evaluated);
        }
    }
    #[test]
    fn test_let_statement(){
        let tests:Vec<(&str,i64)> = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ]; 
        for tt in tests{
            test_integer_object(test_eval(tt.0), tt.1);
        }
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env=Environment::new();
        eval(program.as_any(),&mut env)
    }
    fn test_integer_object(obj: Object, expected: i64) -> bool {
        let result = match obj {
            Object::Integer(i) => Some(i),
            _ => None,
        };
        if result.is_none() {
            println!("object is not Integer. got={}", obj.get_type());
            return false;
        }
        if result.unwrap() != expected {
            println!(
                "object has wrong value. got={}, want ={}",
                result.unwrap(),
                expected
            );
            return false;
        }
        true
    }

    fn test_boolean_object(obj: Object, expected: bool)->bool{
        let result = match obj {
            Object::Boolean(b) => Some(b),
            _ => None,
        };
        if result.is_none() {
            println!("object is not Boolean. got={}", obj.get_type());
            return false;
        }
        if result.unwrap() != expected{
            println!(
                "object has wrong value. got={}, want ={}",
                result.unwrap(),
                expected
            );
            return false;
        }
        true
    }

    fn test_null_object(obj:Object)->bool{
        if Object::Null!=obj{
            println!("object is not null. got={:?}",obj);
        }
        true
    }
}
