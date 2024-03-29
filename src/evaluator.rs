use std::any::{Any, TypeId};

use crate::{
    ast::{Boolean, ExpressionStatement, InfixExpression, IntegerLiteral, PrefixExpression, Program, Statement},
    object::Object,
};

const TRUE: Object= Object::Boolean(true);
const FALSE: Object= Object::Boolean(false);
const NULL:Object = Object::Null;

pub fn eval(node: &dyn Any) -> Object {
    let ret = 
    if (&*node).type_id() == TypeId::of::<Program>(){
        let program = node.downcast_ref::<Program>();
        let binding = program.unwrap().statements.borrow();
        let brow = binding.iter().map(|x|x).collect();
        evaluate_statements(brow)
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
    }else {
        NULL
    };

    ret
}

fn evaluate_statements(stmt:Vec<&Box<dyn Statement>>)->Object{
    let mut result:Object = Object::Null;
    for statement in stmt{
        result = eval(statement.as_any());
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
#[cfg(test)]
mod tests {
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
            // ("true",true),
            // ("false",false),
            // ("1 < 2", true),
            // ("1 > 2", false),
            // ("1 < 1", false),
            // ("1 > 1", false),
            // ("1 == 1", true),
            // ("1 != 1", false),
            // ("1 == 2", false),
            // ("1 != 2", true),
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
}
