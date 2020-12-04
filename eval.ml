open Ast

type time = Reset | Start of float

type value = 
  | VInt of int
  | VBool of bool
  (* | VClosure of id list * expr * env *)

type env = (id * value) list (** Sigma *)

type state = {time: time} (** The dynamic map *)

let initial_env = []

let initial_state = {time = Reset}

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> Bool.to_string b
(* | _ -> "undefined" *)

let string_of_env env =
  failwith "Unimplemented env"

let string_of_state st =
  failwith "Unimplemented state"

let get_val = function
  | (v, _) -> v

(** [to_string v] is s if [v] is the string s, s if [v] is the integer i and 
    s = string_of_int i, "true" if [v] is true, "false" if [v] is false, "undefined" 
    if [v] is undefined, location, closure, extern, or object. *)
let to_string v = 
  match v with 
  | VInt i -> string_of_int i
  | VBool b -> Bool.to_string b
(* | _ -> "undefined" *)


(** [to_bool v] is false is [v] is undefined, false, "", 0. True, otherwise. *)
let to_bool v = 
  match v with 
  | VBool v -> VBool v 
  | VInt 0 -> VBool false 
  | VInt _ -> VBool true
(* | _ -> failwith "You tried to cast a function into a bool" *)

(** [to_int v] is undefined if [v] is undefined, location, closure, extern, or 
    object, i if [v] is the integer i, 1 if [v] is true, 0 if [v] is false, i if 
    [v] is a string s and i = int_of_string s, undefined if [v] is a string s and 
    int_of_string s raises Failure. *)
let to_int v = 
  match v with 
  | VInt i -> VInt i
  | VBool true -> VInt 1 
  | VBool false -> VInt 0
(* | _ -> failwith "You tried to cast a function to an int" *)

let rec eval_expr (e, env, st) = 
  match e with
  | EInt(n) -> (VInt(n), st)
  | EBool(b) -> (VBool(b), st)
  | EVar(x) -> eval_var x env st
  | EUnop (u, e1) -> eval_unop u e1 env st
  | EBinop (b, e1, e2) -> eval_binop b e1 e2 env st
  | ELet (x, e1, e2) -> eval_let x e1 e2 env st
  | EStart -> eval_start st
  | EStop -> eval_stop st
  | EIf (e1, e2, e3) -> eval_if e1 e2 e3 env st
  | ESeq (e1, e2) -> eval_seq e1 e2 env st 
  | _ -> failwith "unimplemented expr"

and eval_if e1 e2 e3 env st =
  (* get the values *)
  let v1, st1 = (e1, env, st) |> eval_expr in
  match v1 |> to_bool, st1 with
  | VBool b, st' -> eval_expr ((if b then e2 else e3), env, st')
  | _ -> failwith "unimplemented"

and eval_seq e1 e2 env st =
  (* get the values *)
  let _, st' = (e1, env, st) |> eval_expr in
  eval_expr (e2, env, st')

and eval_var x env st = 
  match List.assoc_opt x env with 
  | Some value -> (value, st)
  | None -> failwith "free variable"

and eval_unop uop e1 env st =
  let v1 = eval_expr (e1, env, st) |> get_val in 
  match uop with
  | UopNot -> begin 
      match (v1 |> to_bool) with 
      | VBool x -> (VBool (not x), st)
      | _ -> failwith "Unop Not Failure"
    end
  | UopMinus -> begin match (v1 |> to_int) with 
      | VInt i -> (VInt (~- i), st)
      | _ -> failwith "Unop Minus Failure"
    end

and eval_binop bop e1 e2 env st = 
  let math_int_func = simple_maths_helper e1 e2 env st in
  let math_bool_func = simple_bool_helper e1 e2 env st in
  let math_cmp_func = simple_cmp_helper e1 e2 env st in
  match bop with
  | BopPlus -> math_int_func (+)
  | BopMinus -> math_int_func (-)
  | BopTimes -> math_int_func ( * )
  | BopDiv -> math_int_func (/)
  | BopMod -> math_int_func(mod)
  | BopEq -> math_cmp_func (=)
  | BopNeq -> math_cmp_func (!=)
  | BopLt -> math_cmp_func (<)
  | BopLeq -> math_cmp_func (<=)
  | BopGt -> math_cmp_func (>)
  | BopGeq -> math_cmp_func (>=)
  | BopOr -> math_bool_func (||)
  | BopAnd -> math_bool_func (&&)

and simple_maths_helper e1 e2 env st op =
  let v1 = eval_expr(e1, env, st) |> get_val |> to_int in 
  let v2 = eval_expr(e2, env, st) |> get_val |> to_int in 
  match v1, v2 with
  | VInt i1, VInt i2 -> (VInt ((op) i1 i2), st)
  | _ -> failwith "addition doesn't type check"

and simple_bool_helper e1 e2 env st op =
  let v1 = eval_expr(e1, env, st) |> get_val |> to_bool in 
  let v2 = eval_expr(e2, env, st) |> get_val |> to_bool in 
  match v1, v2 with
  | VBool b1, VBool b2 -> (VBool ((op) b1 b2), st)
  | _ -> failwith "addition doesn't type check"

and simple_cmp_helper e1 e2 env st op =
  let v1 = eval_expr(e1, env, st) |> get_val |> to_int in 
  let v2 = eval_expr(e2, env, st) |> get_val |> to_int in 
  match v1, v2 with
  | VInt i1, VInt i2 -> (VBool ((op) i1 i2), st)
  | _ -> failwith "addition doesn't type check"

(** [def_let x (r,st) env] adds the binding of [x] and the value of the expresion 
    [r] in [st] to [env] and returns the new environment.  *)
and def_let x (r,st) env =
  let new_env = (x, get_val (r,st)) :: (env) in
  (r, new_env ,st)

and eval_let x e1 e2 env st = 
  let v, st' = eval_expr (e1, env, st) in
  eval_expr (e2, (x, v)::env, st')

and eval_stop st =
  match st.time with
  | Reset -> failwith "stop called before start"
  | Start t -> (VInt(((Sys.time() -. t) *. 1000.0) |> Int.of_float), 
                {st with time = Reset})

and eval_start st =
  let t = Sys.time() in
  (VInt(t *. 1000.0 |> Int.of_float), {st with time = Start t})

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)

let eval_defn (d, env, st) =
  match d with
  | DLet (x, e) -> let e1 = (e, env, st) |> eval_expr in begin
      match e1 with
      (* | VClosure _ -> def_let x e1 env *)
      | _ -> failwith "functions are unimplemented"
    end
  | _ -> failwith "unimplemented defn"

let eval_phrase (p, env, st) =
  try
    match p with
    | Expr e -> let r, st' =  eval_expr (e, env, st) in (r, env, st')
    | Defn d -> eval_defn (d, env, st)
  with 
  | e -> failwith ((e |> Printexc.to_string) ^ " Exception")