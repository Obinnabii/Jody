(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

(* Variables. *)
type var = string

(* Arithmetic expressions. *)
type aexp =
  | Int of int
  | Var of var
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Times of aexp * aexp
  | Input

(* Boolean expressions. *)
and bexp =
  | True
  | False
  | Equals of aexp * aexp
  | NotEquals of aexp * aexp
  | Less of aexp * aexp
  | LessEq of aexp * aexp
  | Greater of aexp * aexp
  | GreaterEq of aexp * aexp
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

and lst = 
  | VEmpty 
  | VCons of com * lst

(* Commands. *)
and com =
  | Skip
  | Assign of var * aexp
  | Seq of com * com
  | If of bexp * com * com
  | While of bexp * com
  | Print of aexp
  | Test of info * bexp
  | Match of lst * com * com  
  | Switch of lst * aexp 
  | Break
  | Continue

and exp = A of aexp | B of bexp | C of com
