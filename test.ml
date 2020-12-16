(******************************************************************************
   Adapted from Cornell's CS3110 A6
 ******************************************************************************)

open OUnit2
open Ast
open Main

let str_max_int = string_of_int max_int
let str_min_int = string_of_int min_int

let tests = [
  (* These tests show some examples of JoDy Syntax *)
  "int constant", {|42|}, "42";
  "negative int constant", {|-1|}, "-1";
  "true", {|true|}, "true";
  "false", {|false|}, "false";
  "var", {|let x = 0 in x|}, "0";
  "if truthy", {|if true then 5 else 6|}, "5";
  "if falsey", {|if 0 then 3110 else 4110|}, "4110";
  "sequence", {|5+5;10+10|}, "20";
  "let", {|let x = 5 in x + 2|}, "7";
  "#1", {|let x = [5,6] in x.#1 + 2|}, "7";
  "#2", {|let x = [5,6] in x.#2 + 2|}, "8";
  "function app",{|(fun(a) -> a+ 2) 4|}, "6";
  "function app",{|let x = (fun(a b) -> a+ b) in x 3 9|}, "12";
  "rec",{|let rec x(a) = if a<=1 then a else a * x (a-1) in x 4|}, "24";
  "dyn",{|let dyn x(a) = if a<=1 then a else a * x (a-1) in x 4|}, "24";
]

let make_interp_expr_test n in_str out_str =
  n >:: 
  (fun _ -> assert_equal out_str (interp_expr in_str) ~printer:(fun x-> x))

let suite = 
  "suite" >::: 
  List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = 
  run_test_tt_main suite