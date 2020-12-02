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
  "max int", str_max_int, str_max_int;
  "min int", str_min_int, str_min_int;
  "true", {|true|}, "true";
  "false", {|false|}, "false";
  "undefined", {|undefined|}, "undefined";
  "magic word", {|"xyzzy"|}, {|"xyzzy"|};
  "div by 0", {|4/0|}, {|Exception: "Division by zero"|};
  "mod by 0", {|4 mod 0|}, {|Exception: "Division by zero"|};
  "unbound var", {|let x = 0 in y|}, {|Exception: "Unbound variable"|};
  "throw", {|throw 0|}, "Exception: 0";
  "anonymous function", {|fun (x) -> 0|}, "<closure>";
  "apply non-function", {|0 0|}, {|Exception: "Application: not a function"|};
  "apply wrong arity", {|(fun (x) -> 0) 1 2|}, 
  {|Exception: "Application: wrong number of arguments"|};
  "ref", {|ref 0|}, "<location>";
  "assign non location", {|1 := 0|}, {|Exception: "Assignment to non-location"|};
  "object", {|{"x":1}|}, "<object>";
  "length", {|length "bigred"|}, "6";
]

let make_interp_expr_test n in_str out_str =
  n >:: 
  (fun _ -> assert_equal out_str (interp_expr in_str) ~printer:(fun x-> x))

let suite = 
  "suite" >::: 
  List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = 
  run_test_tt_main suite