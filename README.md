

### Token

### Lexer

### Parser

### Evaluator

#### Programs in Monkey are a series of statements.

In monkey language expressions produce values, statements don’t.

#### Statements
##### let statement
    let <identifier> = <expression>;
    let token + identifier + expression
    A let statement in Monkey consists of two changing parts: an identifier and an expression.
##### return statement
    return <expression>;
    return token + expression
##### expression statement
    it’s a statement that consists solely of one expression
    x + 10;
##### block statement
    Block statements are a series of statements enclosed by an opening { and a closing }.



#### Expressions 
##### identifier
    foo * bar / foobar
    foo and bar  are identifier
    
##### integer literal
    <number>
    5
    integer literal as the parameters of a function 
    add(5, 10);
##### boolean literal
    <boolean>
    boolan literal as the expression in a let statement
    let foobar = true;
##### prefix
    -5
    !true
##### infix
    5 + 5
##### if
    if (<condition>) <consequence> else <alternative>
    For example
    if (10 > 5) { true } else { false };
##### function literal
    fn <parameters> <block statement>
    For example, here is a function literal as the expression in a let statement:
    let add = fn(x, y) { return x + y };
    And here is a function literal as the expression in a return statement inside another function literal:
    fn() {
        return fn(x, y) { return x > y; };
    }
    Using a function literal as an argument when calling another function is also possible:
    myFunc(x, y, fn(x, y) { return x > y; });
##### call
    add(2, 3)
##### string literal
##### array literal
##### hash literal
index



### Built-in functions

first
last
rest
len
push
puts