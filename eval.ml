open Ast

type value = 
  | VInt of int
  | VBool of bool
  | VClosure of id list * expr * env

type result = unit (** Output *)

type env = unit (** Sigma *)

type state = unit (** The dynamic map *)

let initial_env = ()

let initial_state = ()

let string_of_value v =
  failwith "Unimplemented"

let string_of_result r =
  failwith "Unimplemented"

let string_of_env env =
  failwith "Unimplemented"

let string_of_state st =
  failwith "Unimplemented"

let eval_expr (e, env, st) =
  | EInt(n) -> (VInt(n), st)
  | EBool(b) -> (VBool(b), st)

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)

let eval_defn (d, env, st) =
  failwith "Unimplemented"

let eval_phrase (p, env, st) =
  failwith "Unimplemented"
