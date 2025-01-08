
<p align="center">
  <a href="https://emptyfridge0900.github.io/interpreter/">
    <picture>
      <img src="https://monkeylang.org/images/logo.png" height="128">
    </picture>
    <h1 align="center">Monkey lang</h1>
  </a>
</p>

# About the project

This project is a rewrite of the code from [Writing An Interpreter In Go]("https://interpreterbook.com/") in Rust. \
Writing an interpreter is fun and challenging, at the same time I thought it would be a good project to practice Rust.
And many people have already implemented monkeylang in other languages. And I got a lot of help from their code. https://monkeylang.org/


## Components
1. Library
    - Lexer
    - Parser
    - Evaluator
2. Console
3. Web


The library is composed of Lexer, Parser, and Evaluator.
## Token
Lexical element of input text
## Lexer
Read a source code and extract tokens from the source code. Another name of lexer is tokenizer
## Parser
Convert tokens into Abstract Syntax Tree. You will see that the methods in the parser object return Statements or Expressions.
## Evaluator
Scan through AST and generate values from it.

# Overview
## Programs 
Programs in Monkey are a series of statements.

In monkey language expressions produce values, statements don’t.

## Statements
### let statement
let \<identifier\> = \<expression\>;

    A let statement in Monkey consists of two changing parts: an identifier and an expression.
    Exampe:
        let foobar = 5
### return statement
return \<expression\>;

    Example:
        return add(x+y);

### expression statement
it’s a statement that consists solely of one expression

    Example:
        x + 10;

### block statement
 Block statements are a series of statements enclosed by an opening { and a closing }.

    Example:
        { true }

## Expressions 
### identifier

    foo * bar / foobar
    foo and bar  are identifier
    
### integer literal
\<number\>

    Example:
        5
    Another example integer literal as the parameters of a function 
        add(5, 10);
### boolean literal
\<boolean\>

    boolan literal as the expression in a let statement
    let foobar = true;
### prefix
-, !

    Examples:
        -5
        !true
### infix
+, -, /, %, *, ==, !=, <=, >=

    Example:
        5 + 5
### if
if (\<condition\>) \<consequence\> else \<alternative\>

    Example:
        if (10 > 5) { true } else { false };
### function literal
fn \<parameters\> \<block statement\>

    For example, here is a function literal as the expression in a let statement:
        ``let add = fn(x, y) { return x + y };``
    And here is a function literal as the expression in a return statement inside another function literal:
        fn() {
            return fn(x, y) { return x > y; };
        }
    Using a function literal as an argument when calling another function is also possible:
        myFunc(x, y, fn(x, y) { return x > y; });
### call
    add(2, 3)
### string literal
\<sequence of characters\>

    "any string"
### array literal
[comma seperated list of expressions]

    Example:
    [1,2,"3",add(4,5)]
### hash literal
{comma seperated list of key-value pairs}

    Example:
    {"name": "Jimmy", "age": 72, "band": "Led Zeppelin"};



## Built-in functions

### first

    let arr = [1,2,3,4];
    first(arr);

>output: 1

### last

    let arr = [1,2,3,4];
    last(arr);

>output: 4

### rest

    let arr = [1,2,3,4];
    rest(arr);

>output: [2,3,4]

### len

    let arr = [1,2,3,4];
    len(arr);

>output: 4

### push

    let arr = [1,2,3,4];
    push(arr,5);

>output: [1,2,3,4,5]

### puts

    let arr = [1,2,3,4];
    puts(arr);

>output: [1,2,3,4]