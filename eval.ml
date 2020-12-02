open Ast

type value = unit

type result = unit

type env = unit

type state = unit

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
  failwith "Unimplemented"

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)

let eval_defn (d, env, st) =
  failwith "Unimplemented"

let eval_phrase (p, env, st) =
  failwith "Unimplemented"
