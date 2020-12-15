# Jody

## Overview
We intend to create a language that has automatic memoization, allowing dynamic programming algorithms to be written using recursion, but actually cache results under the hood.

#### Key Features
- Basic functional programming language features including: constants, variables, first-order functions(regular, anonymous, and recursive), boolean operators, unary operators, basic arithmetic, and if expressions
- Simple but powerful data structures like records, lists, trees, and graphs
- Special system functions
<!-- - A visualization function that shows the state of a dynamic programs’ memoization map after computation. This will serve as an aide for debugging. -->
- A `dyn` function modifier that enables automatic memoization

We intend to build a language with features to help simplify memoization in recursion. Our plan is to implement a language inspired by JoCalf ♰ and adding many features (a complete list is below). The most important of which is the dyn function modifier that automatically saves solutions to recursive problems. Memoization works by using a data structure that maps inputs to previously computed outputs. Our plan is to create this table under the hood of JoDy so that programmers do not actually have to instantiate any tables or make checks to tables. 

#### How to Run

To make the interpreter run `make repl`

Example program
```
|-              // start timer
let x = 3 in x
-|              // end timer (returns ms of CPU time since start)
```

Look in the test file for more code examples.

# Jody Syntax
#### Language
```
(* expressions *)
e ::=
    | i | b                             (* constants *)
    | (e) | begin e end                 (* parenthesized expressions *)
    | x                                 (* variables *)
    | let x = e1 in e2                  (* let expressions *)
    | let rec f (x1 ... xn) = e1 in e2  (* recursive let expressions *)
    | let dyn f (x1 ... xn) = e1 in e2  (* recursive dynamic let expressions *)
    | fun (x1 ... xn) -> e              (* functions *)
    | e0 ... en                         (* application *)
    | if e1 then e2 else e3             (* control flow: if, sequence *)
    | e1; ...; en
    | uop e | e1 bop e2                 (* unary and binary operators *)
    | t                                 (* timer *)
    | p                                 (* pairs *)
    | p.#1                              (* first value in a pair *)
    | p.#2                              (* second value in a pair *)
    
(* Timer Controls *)
t ::=                                   
    | |-                                (* start timer *)
    | -|                                (* end timer *)

(* Pair *)
p ::=
    | [e, e]
    

(* definitions *)
d ::=
    | let x = e                         (* let definition *)
    | let rec f (x1 ... xn) = e         (* recursive let definition *)
    | let dyn f (x1 ... xn) = e         (* recursive dynamic let definitions *)

(* unary operators *)
uop ::=
    | not | - 

(* binary operators *)
bop ::=
    | + | - | * | / | mod
    | < | <= | > | >=
    | = | != | == | !==
    | || | $$

i ::= integers
b ::= booleans
x ::= identifiers
```
#### Commands
```
#quit           ->      Quit the repl
#env            ->      Print the environment representation (shows all the 
                        variable and function assignments)
#display        ->      Toggles the visibility of an inputs memoization status 
                        when running dynamic functions
#lowmem         ->      Toggles low memory mode [#TODO need to explain this @JackRNash]

