use std::any::{Any, TypeId};

use crate::{
    ast::{ExpressionStatement, IntegerLiteral, Node, Program, Statement},
    object::Object,
};

pub fn eval(node: &dyn Any) -> Object {
    let ret = 
    if (&*node).type_id() == TypeId::of::<Program>(){
        let ddd = node.downcast_ref::<Program>();
        let binding = ddd.unwrap().statements.borrow();
        let brow = binding.iter().map(|x|x).collect();
        evaluate_statements(brow)
    } else if (&*node).type_id() == TypeId::of::<ExpressionStatement>(){
        let ddd = node.downcast_ref::<ExpressionStatement>();
        eval(ddd.unwrap().expression.as_ref().unwrap().as_any())
        
    } else if (&*node).type_id() == TypeId::of::<IntegerLiteral>() {
        let ddd = node.downcast_ref::<IntegerLiteral>();
        let ret = ddd.unwrap().integer_value;
        Object::Integer(ret)

    } else {
        Object::Null
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
#[cfg(test)]
mod tests {
    use crate::{ast::Node, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&str, i64)> = vec![("5", 5), ("10", 10)];
        for tt in tests {
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
}
