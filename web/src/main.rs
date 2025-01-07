#![allow(non_snake_case)]
use library::{
    ast::Node, environment::Environment, evaluator::Evaluator, lexer::Lexer, object::Object, parser::Parser
};
use std::{cell::RefCell, rc::Rc};

use dioxus::prelude::*;
use tracing::Level;

static CSS: Asset = asset!("/assets/main.css");
fn main() {
    // Init logger
    dioxus_logger::init(Level::INFO).expect("failed to init logger");
    launch(App);
}

fn App() -> Element {
    // Build cool things ✌️

    rsx! {

        document::Link { rel: "stylesheet", href: CSS }
        //document::Stylesheet { href: CSS }
        Hello{

        }
    }
}

pub fn test (input:&str) {


}
static helloworld:&str = "puts(\"hello world\")";

static builtins:&str =r#"let map ={
    true:[1,2,3,4,5,6],
    false:"hello world"
};

puts(map[true]);
puts(map[true][0]);
puts(map[true][10]);
puts(first(map[true]));
puts(last(map[true]));
puts(len(map[true]));
puts(len(map[false]));
let a =[1];
let b= push(a,2)
puts(b);
puts(rest(b));"#;

static factorial:&str = r#"let factorial = fn(n) {
  if (n <= 1) {
    return 1;
  }
  return n * factorial(n - 1);
}
puts(factorial(5))"#;

static fizzbuzz:&str=r#"let fizzbuzz = fn(n) {
    if (n == 1) {
        puts(1);
        return 0;
    }

    if (n % 15 == 0) {
        puts("fizzbuzz");
        return fizzbuzz(n-1);
    }
    if (n % 5 == 0) {
        puts("buzz");
        return fizzbuzz(n-1);
    }
    if (n % 3 == 0) {
        puts("fizz");
        return fizzbuzz(n-1);
    }

    puts(n);
    fizzbuzz(n-1);
};

fizzbuzz(100);"#;

static double_map:&str = r#"let map = fn(arr, f) {
    let iter = fn (arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    iter(arr, []);
};

let a = [1, 2, 3, 4];
let double = fn(x) { x * 2 };

puts("Before double: ", a);
puts("After double: ", map(a, double));"#;

pub fn Hello() -> Element {
    let mut user_input = use_signal::<String>(|| "puts(\"hello world\")".to_string());
    let mut result = use_signal(|| "".to_string());
    let mut output = use_signal::<Vec<String>>(||vec![]);
    let xx = move |str:&str|{
        output.push(str.to_owned());
    };
    let func =   Rc::new(RefCell::new(xx));
    rsx! {
            div{
                class:"input-section",
                div {  
                    class:"tabs",
                    button { onclick: move|_|{ user_input.set(helloworld.to_string()) }, "Hello World" },
                    button { onclick: move|_|{ user_input.set(builtins.to_string()) }, "Built-in" },
                    button { onclick: move|_|{ user_input.set(factorial.to_string()) }, "Factorial" },
                    button { onclick: move|_|{ user_input.set(fizzbuzz.to_string()) }, "FizzBuzz" },
                    button { onclick: move|_|{ user_input.set(double_map.to_string()) }, "Double with Map" },
                },
                textarea{
                    id:"codeInput",
                    value:"{user_input}",
                    oninput: move |event| user_input.set(event.value())
                },
                button{
                    class:"run-button",
                    onclick: move |_| {
                        let val = user_input.read().clone();
                        output.set(vec![]);
                        evaluate(val,func.clone());
                    },
                    "Run"
                }
            },
            div{
                class:"output-section",
                id:"output",
                
                for n in output.iter(){
                    div{
                        class:"output-line success",
                        "{n}"
                    }
                }
            }
    }
}

fn evaluate<'a>(input: String, output:Rc<RefCell<dyn FnMut(&str) + 'a>>){

    let mut env = Environment::new();
    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    if p.errors().len() != 0 {
        print_parser_error(p.errors,output.clone());
    }

    let evaluator = Evaluator::new(output.clone());
    let evaludated = evaluator.eval(&Node::Program(program), &mut env);
    if evaludated != Object::Null {
         evaludated.inspect();
    }else{
        output.borrow_mut()("Null");
    }
}
fn print_parser_error<'a>(errors: Vec<String>,output:Rc<RefCell<dyn FnMut(&str) + 'a>>) {

    output.borrow_mut()("Woops! We ran into some monkey business here!");
    output.borrow_mut()("parser errors:");
    for msg in errors{
        output.borrow_mut()(&msg);
    }
}
