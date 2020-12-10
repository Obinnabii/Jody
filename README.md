# Jody

To make the interpreter run `make repl`

Example program
```
|-              // start timer
let x = 3 in x
-|              // end timer (returns ms of CPU time since start)
```

Look in the test file for code examples.

# Jody Syntax

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
    | if e1 then e2 else e3             (* control flow: if, sequence, while *)
    | e1; ...; en
    | while e1 do e2 done
    | uop e | e1 bop e2                 (* unary and binary operators *)
    | |- | -|                           (* start and end timer *)
    

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