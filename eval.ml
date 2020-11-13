(* CS 4110 Homework 3
   This is the file where you'll do your work. Your job is to take an AST
   (which has already been parsed for you) and execute it. *)

open Ast

(* Interpreter exceptions. *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var

(* A type for stores. 
   Invariant: [breaks] >= 0*)
type store = {vars: (var * int) list; breaks: int} 

(** A type for configurations. ⟨𝜎, 𝑐, 𝑐′, 𝜅⟩ *)
type configuration = store * com * com  * com list

(* Create an initial configuration from a command. *)
let make_configuration (c:com) : configuration =
  ({vars=[]; breaks=0}, c, Skip, [])

(* Evaluate a command. *)
let rec evalc ((st, com1, com2, com_list):configuration) : store = 
  match com1, com2 with
  | Skip, Skip -> st
  | Skip, next_com  -> evalc (st, next_com, Skip, com_list)
  | Seq (c1, c2), Skip -> evalc (st, c1, c2, com_list)
  | Seq (c1, c2), next_com -> evalc (st, c1,  Seq (c2, next_com), com_list) 
  | Assign (var, aexp), next_com  -> 
    let st' = {st with vars = (var, evala aexp st)::st.vars}  in
    evalc (st', next_com, Skip, com_list)
  | If (bexp, c1, c2), next_com -> 
    begin
      match evalb bexp st with
      | true -> evalc (st, c1, next_com, com_list) 
      | false -> evalc (st, c2, next_com, com_list)
    end
  | While (bexp, com), next_com ->
    let com_list' = (next_com)::com_list in
    let st' = while_helper (st, bexp, com, com_list') in
    begin match st'.breaks > st.breaks with (* we have encountered a break statement and eval'd *)
      | true -> evalc ({st' with breaks=st'.breaks-1}, Skip, Skip, com_list)
      | false ->  evalc (st', next_com, Skip, com_list)
    end
  | Break, _ -> 
    begin 
      match com_list with
      | next_cmd::t -> evalc ({st with breaks=st.breaks+1}, next_cmd, Skip, t)
      | [] -> raise IllegalBreak
    end
  | Continue, _ ->  
    begin 
      match com_list with
      | _::t -> st
      | [] -> raise IllegalContinue
    end
  | Test (info, b), next_com -> 
    begin 
      match evalb b st with
      | true -> evalc (st, next_com, Skip, com_list)
      | false -> print_endline "TestFailed"; print_info info; st 
    end
  | Print aexp, next_com -> 
    string_of_int (evala aexp st) |> print_endline; 
    evalc (st, next_com, Skip, com_list)

and print_info (info:info) : unit =
  match info with
  | ((l1,c1),(l2,c2)) -> 
    let stringify l c = "Line: "^ string_of_int l^
                        "; Character: "^ string_of_int c in
    (stringify l1 c1) ^" -> "^ (stringify l2 c2) |> print_endline;

and while_helper (st, guard, body, cmd_list) : store  =
  match evalb guard st, st.breaks with
  | true, 0 -> let st' = evalc (st, body, Skip, cmd_list) in 
    while_helper(st', guard, body, cmd_list) (* n will be positive *)
  | _ -> st


and evala (arith:aexp) (s: store) : int = 
  match arith with
  | Int i -> i
  | Var x -> 
    begin
      match List.assoc_opt x s.vars with
      | Some i -> i
      | None -> raise (UnboundVariable x) 
    end
  | Plus(aexp1, aexp2) -> (evala aexp1 s) + (evala aexp2 s)
  | Minus(aexp1, aexp2) -> (evala aexp1 s) - (evala aexp2 s)
  | Times(aexp1, aexp2) -> (evala aexp1 s) * (evala aexp2 s)
  | Input -> print_string "> "; read_int () 

and evalb (b:bexp) (s:store) : bool =
  match b with
  | True -> true
  | False -> false
  | Equals (aexp1, aexp2) -> evala aexp1 s = evala aexp2 s
  | NotEquals (aexp1, aexp2) -> evala aexp1 s <> evala aexp2 s
  | Less (aexp1, aexp2) -> evala aexp1 s < evala aexp2 s
  | LessEq (aexp1, aexp2) -> evala aexp1 s <= evala aexp2 s
  | Greater (aexp1, aexp2) -> evala aexp1 s > evala aexp2 s
  | GreaterEq (aexp1, aexp2) -> evala aexp1 s >= evala aexp2 s
  | Not bexp -> not (evalb bexp s)
  | And (bexp1, bexp2) -> 
    let b1 = evalb bexp1 s in
    let b2 = evalb bexp2 s in
    b1 && b2
  | Or (bexp1, bexp2) -> 
    let b1 = evalb bexp1 s in
    let b2 = evalb bexp2 s in
    b1 || b2



