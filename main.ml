(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

open Ast
open Eval

let interp_expr s =
  try
    s
    |> Parse.parse_expr
    |> Eval.eval_expr_init
    |> (fun (v, _) -> string_of_value v)
  with
    Parse.SyntaxError s | Eval.RunTimeError s -> s

let interp_phrase (s, env, st) =
  try
    s
    |> Parse.parse_phrase
    |> (fun p -> Eval.eval_phrase (p, env, st))
    |> (fun (v, env', st') -> (string_of_value v, env', st'))
  with
  | Parse.SyntaxError s | Eval.RunTimeError s -> (s, env, st)
  | End_of_file -> ("", env, st)
