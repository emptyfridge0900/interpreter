use std::any::{Any, TypeId};

use crate::{
    ast::{BlockStatement, Boolean, ExpressionStatement, IfExpression, InfixExpression, IntegerLiteral, Node, PrefixExpression, Program, ReturnStatement, Statement},
    object::Object,
};

const TRUE: Object= Object::Boolean(true);
const FALSE: Object= Object::Boolean(false);
const NULL:Object = Object::Null;

pub fn eval(node: &dyn Any) -> Object {
    let ret = 
    if (&*node).type_id() == TypeId::of::<Program>(){
        let program = node.downcast_ref::<Program>();
        eval_program(program.unwrap())
    } else if (&*node).type_id() == TypeId::of::<ExpressionStatement>(){
        let exp_stmt = node.downcast_ref::<ExpressionStatement>();
        eval(exp_stmt.unwrap().expression.as_ref().unwrap().as_any())

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
        let right = eval(x.right.as_ref().unwrap().as_any());
        eval_prefix_expression(&prefix_exp.unwrap().operator,right)
    } else if (&*node).type_id() == TypeId::of::<InfixExpression>(){
        let infix_exp = node.downcast_ref::<InfixExpression>();
        let x =infix_exp.unwrap();
        let right = eval(x.right.as_ref().unwrap().as_any());
        let left = eval(x.left.as_ref().unwrap().as_any());
        eval_infix_expression(&infix_exp.unwrap().operator,left,right)
    } else if (&*node).type_id() == TypeId::of::<BlockStatement>(){
        let statement = node.downcast_ref::<BlockStatement>();
        let x = statement.unwrap();
        //eval_statements(x.statements.iter().map(|y|y).collect())
        eval_block_statement(x)
    } else if (&*node).type_id() == TypeId::of::<IfExpression>(){
        let if_exp = node.downcast_ref::<IfExpression>();
        eval_if_expression(if_exp.unwrap())
    } else if (&*node).type_id() == TypeId::of::<ReturnStatement>(){
        let ret_statement = node.downcast_ref::<ReturnStatement>();
        let val = eval(ret_statement.unwrap().return_value.as_ref().unwrap().as_any());
        Object::Return(Box::new(val))
    }else {
        Object::Unkown
    };

    ret
}

fn eval_program(program:&Program)->Object{
    let binding = program.statements.borrow();
    let stmt:Vec<&Box<dyn Statement>> = binding.iter().map(|x|x).collect();
    let mut result:Object = Object::Null;
    for statement in stmt{
        result = eval(statement.as_any());
        if let Object::Return(value) = result{
            return *value;
        }
    }
    result
}
fn eval_block_statement(block:&BlockStatement)->Object{
    let mut result:Object = Object::Null;
    for statement in &block.statements{
        result = eval(statement.as_any());
        if result.get_type() == "RETURN_VALUE"{
            return result;
        }
        if let Object::Return(value)=result{
            return *value
        }
    }
    NULL

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
        _=>NULL
    }
}
fn eval_infix_expression(operator:&str,left:Object,right:Object)->Object{
    if operator == "=="{
        native_boolean_to_boolean_object(left==right)
    } else if operator == "!="{
        native_boolean_to_boolean_object(left != right)
    } else if let (Object::Integer(l), Object::Integer(r))=(left,right){
        eval_integer_infix_expression(operator,l,r)
    } else {
        NULL
    }
    // match (left,right){
    //     (Object::Integer(l),Object::Integer(r))=>eval_integer_infix_expression(operator,l,r),
    //     _=>NULL
    // }
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
        _=>NULL
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
        _=>NULL
    }
}
fn eval_if_expression(ie:&IfExpression)->Object{
    let condition = eval(ie.condition.as_ref().unwrap().as_any());
    if is_truthy(condition){
        eval(ie.consequence.as_ref().unwrap().as_any())
    } else if ie.alternative.is_some(){
        eval(ie.alternative.as_ref().unwrap().as_any())
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
#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::{ast::Node, lexer::Lexer, object::Object, parser::Parser};

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
            ("if (1 < 2) { 10 }", Box::new(10)),
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

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        eval(program.as_any())
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
