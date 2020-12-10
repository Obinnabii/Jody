(******************************************************************************
   Adapted from Cornell's CS3110 A6
 ******************************************************************************)

open OUnit2
open Ast
open Main

let str_max_int = string_of_int max_int
let str_min_int = string_of_int min_int

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)
let tests = [
  (* All of these tests will currently fail because you have not yet
     implemented interpretation of any of these syntactic forms. A
     completed interpreter should pass all of them, though. *)
  "int constant", {|42|}, "42";
  "negative int constant", {|-1|}, "-1";
  "true", {|true|}, "true";
  "false", {|false|}, "false";
  "var", {|let x = 0 in x|}, "0";
  "if truthy", {|if true then 5 else 6|}, "5";
  "if falsey", {|if 0 then 3110 else 4110|}, "4110";
  "sequence", {|5+5;10+10|}, "20";
]

let make_interp_expr_test n in_str out_str =
  n >:: 
  (fun _ -> assert_equal out_str (interp_expr in_str) ~printer:(fun x-> x))

let suite = 
  "suite" >::: 
  List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = 
  run_test_tt_main suite