(** Implements the big-step environment model semantics. *)

open Ast

(** Raised to signal a Runtime error. *)
exception RunTimeError of string

(** [value] is the type of JoDy values *)
type value

(** [env] is an environment, which maps identifiers to values *)
type env

(** [state] is a state, which maps locations to values *)
type state

(** [initial_env] is the environment in which evaluation begins. *)
val initial_env : env

(** [initial_state] is the state in which evaluation begins. *)
val initial_state : state

(** [string_of_value v] is a string representing value [v].
    - If v is an int, that string should be [string_of_int v].
    - If v is a boolean, that string should be [string_of_bool b].
    - If v is a closure, that string should be [<fun>].
    - If v is an recursive closure, that string should be [<rec fun>].
*)
val string_of_value : value -> string

(** [string_of_env env] is a string representing environment [env].
    Shows all the functions and variables stored in the environment at any given
    time*)
val string_of_env : env -> string

(** [string_of_state st] is a string representing state [st].
    It is up to you how to construct that string; it will only
    be used in the REPL, not in any test cases. *)
val string_of_state : state -> string

(** [eval_expr_init e] is [r,st] if [e] in the initial environment and state,
    evaluates to [r,st], that is, <e, initial_env, initial_state> ==> <r, st> *)
val eval_expr_init : expr -> value * state

(** [eval_expr (e, env, st)] is [(r, st')] if <e, env, st> ==> <r, st'> *)
val eval_expr : expr * env * state -> value * state

(** [eval_defn (d, env, st)] is [(r, env', st')] if
    <d, env, st> ==> <r, env', st'> *)
val eval_defn : defn * env * state -> value * env * state

(** [eval_phrase (p, env, st)] is [(r, env', st')] if
    <p, env, st> ==> <r, env', st'> *)
val eval_phrase : phrase * env * state -> value * env * state

val toggle : state -> string -> state

val status : state -> string -> string
(* val modes : string list *)