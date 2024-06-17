use core::panic;
use std::{any::{Any, TypeId}, collections::HashMap};

use crate::{
    ast::{Expression, Identifier, Node, Program, Statement}, builtin::Builtins, environment::{new_enclosed_environment, Environment}, object::Object
};

const TRUE: Object= Object::Boolean(true);
const FALSE: Object= Object::Boolean(false);
const NULL:Object = Object::Null;

pub fn eval(node: &Node,env:&mut Environment) -> Object {
    match node {
        Node::Program(program)=>eval_program(program,env),
        Node::Statement(statement)=> eval_statement(statement,env),
        Node::Expression(expression)=>eval_expression(&*expression,env)
    }

}

fn eval_program(program:&Program,env:&mut Environment)->Object{
    let mut result:Object = Object::Null;

    for statement in &program.statements{
        result = eval_statement(statement,env);
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
fn eval_statement(statement:&Statement,env:&mut Environment)->Object{
    match statement {
        Statement::Let { token, ident, value }=>{
            let val = eval_expression(value, env);
            env.set(ident.name.clone(), val.clone());
            val
        },
        Statement::Return { token, value }=>Object::Return(Box::new(eval_expression(value, env))),
        Statement::Expression { token, expression }=>eval_expression(expression, env),
        Statement::Block { token, statements }=>eval_block_statement(&statement, env),
    }
}
fn eval_expression(expression:&Expression,env:&mut Environment)->Object{
    match expression{
        Expression::Identifier(ident)=>eval_identifier(&ident, env),
        Expression::IntegerLiteral { value }=>Object::Integer(*value),
        Expression::Boolean { value }=>Object::Boolean(*value),
        Expression::Prefix { token, operator, right }=> eval_prefix_expression(&operator, eval_expression(right,env)),
        Expression::Infix { token, left, operator, right }=>eval_infix_expression(&operator, eval_expression(left,env), eval_expression(right,env)),
        Expression::If { condition, consequence, alternative }=>eval_if_expression(&expression, env),
        Expression::FunctionLiteral { token, parameters, body }=>{
            Object::Function { parameters: parameters.to_vec(), body: body.as_ref().clone(), env:env.clone() }
        },
        Expression::Call { token, function, arguments }=>{
            let func = eval_expression(function,env);
            if is_error(func.clone()){
                return func;
            }
            let args = eval_expressions(arguments, env);
            if args.len() ==1 && is_error(args[0].clone()){
                return args[0].clone();
            }
            apply_function(func,args)
        },
        Expression::StringLiteral { token, value }=>Object::String(value.to_string()),
        Expression::ArrayLiteral { token, elements }=>{
            let elements = eval_expressions(elements, env);
            if elements.len() ==1 && is_error(elements[0].clone()){

            }
            Object::Array(elements.into())
        },
        Expression::Index { left, index }=>{
            let left = eval_expression(left,env);
            if is_error(left.clone()){
                return left;
            }
            let index = eval_expression(index,env);
            if is_error(index.clone()){
                return index;
            }
            return eval_index_expression(left,index);
        },
        Expression::HashLiteral { pairs }=>eval_hash_literal(pairs,env),
        _|Expression::Error=>Object::Null
    }
}
fn eval_hash_literal(hash_pairs:&HashMap<Expression,Expression>,env: &mut Environment)->Object{
    let mut pairs:HashMap<Object,Object> = HashMap::new();

    for (key,value) in hash_pairs{
        let key = eval_expression(key, env);

        let value = eval_expression(value, env);

        pairs.insert(key, value);
    }

    Object::Hash(pairs)
}

fn eval_block_statement(block:&Statement,env:&mut Environment)->Object{
    let mut result:Object = Object::Null;
    if let Statement::Block { token, statements }=block{

        for statement in statements{
            result = eval(&Node::Statement(statement.clone()),env);
    
            match result{
                Object::Return(val)=>return Object::Return(val),
                Object::Error(message)=>return Object::Error(message),
                _=>continue
            }
        }
    }
    result
}
fn eval_expressions(exps:&Vec<Expression>,env:&mut Environment)->Vec<Object>{
    let mut result:Vec<Object> = vec![];
    for e in exps{
        let evaluated=  eval_expression(e,env);
        if is_error(evaluated.clone()){
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn apply_function(func:Object, args:Vec<Object>)->Object{
    match func{
        Object::Function { parameters, body, env }=>{
            let mut extended_env= extend_function_env(parameters, env, args);
            let evaluated=eval(&Node::Statement(body),&mut extended_env);
            unwrap_return_value(evaluated)
        },
        Object::Builtin(fnunction)=> fnunction(args),
        _=> new_error("format".to_owned())
    }
    
}
fn extend_function_env(parameters:Vec<Identifier>,env:Environment,args:Vec<Object>)->Environment{
    let mut env = new_enclosed_environment(env);
    for (i,param) in parameters.iter().enumerate(){
        env.set(param.name.clone(), args[i].clone());
    }
    env
}
fn unwrap_return_value(obj:Object)->Object{
    if let Object::Return(val)=obj{
        return *val
    }
    obj
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
    } else if left.get_type() =="STRING".to_owned() && right.get_type() == "STRING".to_owned(){
        eval_string_infix_expression(operator.to_string(), left,right)
    } else {
        new_error(format!("unknown operator: {} {} {}",left.get_type(),operator,right.get_type()))
    } 
    // match (left,right){
    //     (Object::Integer(l),Object::Integer(r))=>eval_integer_infix_expression(operator,l,r),
    //     _=>NULL
    // }
}
fn eval_string_infix_expression(operator:String,left:Object,right:Object)->Object{
    if operator != "+"{
        return new_error(format!("unknown operator: {} {} {}",left.get_type(),operator.to_string(), right.get_type()));
    }
    let mut left_val=if let Object::String(v)=left{
        v
    }else{
        panic!("not a string")
    };
    let right_val = if let Object::String(v) = right {
        v
    }else{
        panic!("not a string")
    };
    left_val.push_str(&right_val);
    Object::String(left_val)

}
fn eval_identifier(node:&Identifier,env:&Environment)->Object{
    let val = env.get(&node.name.clone());
    if val.is_some(){
        return val.unwrap();
    }
    let builtin = Builtins::new();
    if let Object::Builtin(b)= builtin.get(node.name.to_string()){
        return Object::Builtin(b);
    }
    return new_error(format!("identifier not found: {}",node.name));
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
fn eval_if_expression(ie:&Expression,env:&mut Environment)->Object{
    if let Expression::If { condition, consequence, alternative }=ie{
        
        let condition = eval(&Node::Expression(condition.clone()),env);
        if is_error(condition.clone()){
            return condition;
        }
        if is_truthy(condition){
            eval(&Node::Statement(consequence.as_ref().clone()),env)
        } else if alternative.is_some(){
            eval(&Node::Statement(alternative.as_ref().unwrap().as_ref().clone()),env)
        } else {
            NULL
        }
    }else{
        Object::Error("not if expression".to_owned())
    }
}
fn eval_index_expression(left:Object, index:Object)->Object{
    match (left.clone(),index.clone()){
        (Object::Array(array),Object::Integer(int))=>eval_array_index_expression(left,index),
        (Object::Hash(pairs),_)=>eval_hash_index_expression(pairs,index),
        _=>new_error(format!("index operator not supported {:?}",left))
    }
}

fn eval_hash_index_expression(pair:HashMap<Object,Object>,index:Object)->Object{
    let value = pair.get(&index);
    if value.is_none(){
        return new_error(format!("unusable as hash key"));
    }
    value.unwrap().clone()
}

fn eval_array_index_expression(array:Object, index:Object)->Object{
    if let Object::Array(array_object)=array{
        let max:i64 = (array_object.len()-1).try_into().unwrap();
        if let Object::Integer(idx)=index{
            if idx<0 || idx>max{
                return Object::Null;
            }else{
                return array_object[idx as usize].clone();
            }
        }
    }
    Object::Null
}

fn is_truthy(obj:Object)->bool{
    match obj{
        NULL=>false,
        TRUE=>true,
        FALSE=>false,
        _=>true
    }
}
pub fn new_error(format:String)->Object{
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
    use core::{panic};
    use std::{any::Any, collections::HashMap};

    use crate::{ast::{Expression, Node}, environment::Environment, lexer::Lexer, object::Object, parser::Parser};

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
            ),
            (
                r#""Hello" - "World""#,
                "unknown operator: STRING - STRING"
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key"
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
    #[test]
    fn test_function_object(){
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        if let Object::Function { parameters, body, env }=evaluated{
            if parameters.len()!=1{
                panic!("function has wrong parameters");
            }
            if parameters[0].string() != "x"{
                panic!("parameter is not 'x'. got={}",parameters[0].string());
            }
            let expect= "(x + 2)";
            if body.string() != expect{
                panic!("body is not {}. got={}",expect,body.string());
            }
        }else{
            panic!("object is not Function. got {:?}",evaluated);
        }
    }
    #[test]
    fn test_function_application(){
        let tests:Vec<(&str,i64)> = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for tt in tests{
            test_integer_object(test_eval(tt.0), tt.1);
        }
    }
    #[test]
    fn test_string_concatenation(){
        let input=r#""Hello"+" "+"World!""#;
        let evaluated = test_eval(input);
        if let Object::String(v)=evaluated.clone(){
            if v != "Hello World!"{
                println!("string has wrong value. got={}",v);
            }
        }else{
            panic!("object is not String. got={:?}",evaluated);
        }
    }

    #[test]
    fn test_builtin_functions(){
        let test:Vec<(&str,Object)> = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (r#"len(1)"#, Object::String("argument to `len` not supported, got INTEGER".to_string())),
            (r#"len("one", "two")"#, Object::String("wrong number of arguments. got=2, want=1".to_string()))
        ];

        for tt in test{
            let evaluated = test_eval(tt.0);
            match tt.1{
                Object::Integer(v)=> {
                    test_integer_object(evaluated, v);
                },
                Object::String(v)=>{
                    if let Object::Error(e)=evaluated{
                        if v!=e{
                            println!("wrong error message. expected={}, got={}",v,e);
                        }
                    }else{
                        println!("object is not Error. got={:?}",evaluated);
                        continue
                    }
                },
                _=>unreachable!()
            }
        }

    }

    #[test]
    fn test_array_literals(){
        let input ="[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        if let Object::Array(x) = evaluated{
            if x.len()!=3{
                panic!("array has wrong num of elements. got={}",x.len());
            }
            test_integer_object(x[0].clone(), 1_i64);
            test_integer_object(x[1].clone(), 1_i64);
            test_integer_object(x[2].clone(), 1_i64);
        }else{
            panic!("object is not array.")
        }
    }
    #[test]
    fn test_array_index_expression(){
        let tests:Vec<(&str,Object)> = vec![
            ("[1, 2, 3][0]",Object::Integer(1_i64)),
            ("[1, 2, 3][1]",Object::Integer(2_i64)),
            ("[1, 2, 3][2]",Object::Integer(3_i64)),
            ("let i = 0; [1][i];", Object::Integer(1_i64)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3_i64)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3_i64)),
            ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Object::Integer(6_i64)),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Object::Integer(2_i64)),
            ("[1, 2, 3][3]",Object::Error("".to_string())),
            ("[1, 2, 3][-1]",Object::Error("".to_string()))
        ];

        for tt in tests{
            let evaluated = test_eval(tt.0);
            match tt.1{
                Object::Integer(i)=>test_integer_object(evaluated, i),
                _=>test_null_object(evaluated)
            };
        }
    }
    #[test]
    fn test_hash_literals(){
        let input = r#"let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#;
        let evaluated = test_eval(input);
        if let Object::Hash(pairs)=evaluated{
            let expectd:HashMap<Object,i64> = HashMap::from([
               (Object::String("one".to_string()),1), 
               (Object::String("two".to_string()),2), 
               (Object::String("three".to_string()),3), 
               (Object::Integer(4),4), 
               (Object::Boolean(true),5), 
               (Object::Boolean(false),6), 
            ]);
            for (key,exp_value) in expectd{
                let p = pairs.get(&key);
                if p.is_none(){

                }
                let p = p.unwrap();
                test_integer_object(p.clone(), exp_value);

            }
        }else{
            panic!("Eval didn't return Hash. got={:?}",evaluated);
        }
    }
    #[test]
    fn test_hash_index_expression(){
        let tests:Vec<(&str,Option<i64>)> = vec![
            (r#"{"foo": 5}["foo"]"#,Some(5)),
            (r#"{"foo": 5}["bar"]"#,None),
            (r#"let key = "foo"; {"foo": 5}[key]"#,Some(5)),
            (r#"{}["foo"]"#,None),
            (r#"{5: 5}[5]"#,Some(5)),
            (r#"{true: 5}[true]"#,Some(5)),
            (r#"{false: 5}[false]"#,Some(5)),
        ];
        for tt in tests{
            let evaluated = test_eval(tt.0);

            if tt.1.is_some(){
                test_integer_object(evaluated.clone(), tt.1.unwrap());
            }else{
                test_null_object(evaluated);
            }
        }
    }


    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env=Environment::new();
        eval(&Node::Program(program),&mut env)
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
