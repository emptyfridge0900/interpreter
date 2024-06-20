#![allow(non_snake_case)]
use library::{
    ast::Node,
    environment::Environment,
    lexer::{self, Lexer},
    object::Object,
    parser::{self, Parser},
};
use std::thread::Scope;

use dioxus::prelude::*;
use tracing::Level;

fn main() {
    // Init logger
    dioxus_logger::init(Level::INFO).expect("failed to init logger");
    launch(App);
}

fn App() -> Element {
    // Build cool things ✌️

    rsx! {
        link { rel: "stylesheet", href: "main.css" }
        Hello{

        }
    }
}

pub fn Hello() -> Element {
    let mut user_input = use_signal::<String>(|| "puts(\"hello world\")".to_string());
    let mut result = use_signal(|| "".to_string());
    rsx! {
        div{
            id:"content-wrap",
            textarea{
               id:"user-input",
               cols:70,
               rows:20,
               value:"{user_input}",
               oninput: move |event| user_input.set(event.value())
            }
            button{
                onclick: move |_| {
                    let val = user_input.read().clone();
                    let e = evaluate(val);
                    *result.write() = e;
                },
                "Run"
            }
            textarea{
                id:"output",
                cols:70,
                rows:20,
                value:"{result}"
            }
        }

    }
}

fn evaluate(input: String) -> String {
    let mut env = Environment::new();
    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    if p.errors().len() != 0 {
        return print_parser_error(p.errors);
    }
    println!("{}", program.string());
    let evaludated = library::evaluator::eval(&Node::Program(program), &mut env);
    if evaludated != Object::Null {
        return evaludated.inspect();
    }
    "???".to_string()
}
fn print_parser_error(errors: Vec<String>) -> String {
    let mut vec: Vec<String> = vec![];
    let mut s = String::from(MONKEY_FACE);
    s.push('\n');
    s.push_str("Woops! We ran into some monkey business here!");
    s.push('\n');
    s.push_str("parser errors:");
    s.push('\n');
    println!("{}", MONKEY_FACE);
    println!("Woops! We ran into some monkey business here!");
    println!("parser errors:");

    for msg in errors {
        println!("{}", msg);
        s.push_str(&msg);
        s.push('\n');
    }
    s
}

const MONKEY_FACE: &str = r#"
          __,__
 .--.  .-"     "-.  .--.
/ .. \/ .-. .-.   \/ .. \
| | '| /   Y   \  |'  | |
| \ \ \  0 | 0 / /    / |
\ '- ,\.-"""""""-./, -' /
''-' /  _ ^ ^ _   \ '-''
    |  \._   _./   |
    \   \ '~' /   /
    '._  '-=-' _.'
       '-----'"#;
