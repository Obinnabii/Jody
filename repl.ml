(******************************************************************************
   Adapted from Cornell's CS3110 A6
 ******************************************************************************)

(* Acknowledgement:
 * This JoCaml REPL is adapated from the sample REPL provided as part of the
 * Lambda-Term package (c) 2015 by Martin DeMello, released under BSD3. *)

open React
open Lwt
open LTerm_text

exception Quit

module Interpreter = struct
  type repl_state = {
    command_count : int;
    env : Eval.env;
    st : Eval.state;
  }

  let initial_rstate = {
    command_count = 1;
    env = Eval.initial_env;
    st = Eval.initial_state;
  }

  (* LOADING FILES
     \([A-Z_a-z/.]+\).jd
     https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string *)
  (* SPLITTING and REPLACING
     https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html *)

  let quit_regex  = Str.regexp {|^#quit\(;;\)?$|}
  let env_regex   = Str.regexp {|^#env\(;;\)?$|}
  let state_regex = Str.regexp {|^#state\(;;\)?$|}
  let low_mem_regex = Str.regexp {|^#lowmem\(;;\)?$|}
  let display_regex = Str.regexp {|^#display\(;;\)?$|}
  let load_regex = Str.regexp {|^#load\( \)+\([A-Z_a-z/.]+\).jd\(;;\)?$|}
  let file_regex = Str.regexp {|\([A-Z_a-z/.]+\).jd|}

  let load_loop_acc = (" ", Eval.initial_env, Eval.initial_state)

  let matches s r =
    Str.string_match r s 0

  let search s r =
    Str.search_forward r s 0;
    Str.matched_string s

  let get_file filename = 
    (* The following code to read in a file taken from 
       https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string *)
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let split_file file = Str.split (Str.regexp "[\n]") file

  let rec load_loop ((_, env, st) as acc) lines =
    match lines with
    | h :: t -> load_loop (Main.interp_phrase (h, env, st)) t
    | [] -> acc

  let out_load state (out, env, st) =
    let state' = {
      command_count = state.command_count;
      env = Eval.combine_env env state.env;
      st = Eval.combine_state st state.st;
    } in
    (state', out)

  let load_file s state = 
    try
      file_regex
      |> search s 
      |> get_file 
      |> split_file
      |> load_loop load_loop_acc
      |> out_load state
    with
    | _ -> (state, "FILE NOT FOUND") 


  let eval state s =
    if matches s quit_regex then
      raise Quit
    else if matches s env_regex then
      (state, Eval.string_of_env state.env)
    else if matches s state_regex then
      (state, Eval.string_of_state state.st)
    else if matches s low_mem_regex then
      let state' = {state with st = Eval.toggle state.st "low_mem_mode"} in
      (state', Eval.status state'.st "low_mem_mode")
    else if matches s display_regex then
      let state' = {state with st = Eval.toggle state.st "display"} in
      (state', Eval.status state'.st "display")
    else if matches s load_regex then
      load_file s state
    else
      let (out, env', st') = Main.interp_phrase (s, state.env, state.st) in
      let state' = {
        command_count = state.command_count + 1;
        env = env';
        st = st';
      } in
      (state', out)
end

let make_prompt state =
  let prompt = "# " in
  eval [ S prompt ]

let make_output state out =
  let output =
    if out = "" then "\n"
    else Printf.sprintf "%s\n\n" out in
  eval [ S output ]

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.const (make_prompt state))
end

let rec loop term history state =
  Lwt.catch (fun () ->
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
      rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    let command_utf8 = Zed_string.to_utf8 command in
    let state, out = Interpreter.eval state command_utf8 in
    LTerm.fprints term (make_output state out)
    >>= fun () ->
    LTerm_history.add history command;
    loop term history state
  | None ->
    loop term history state

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch (fun () ->
      let state = Interpreter.initial_rstate in
      Lazy.force LTerm.stdout
      >>= fun term ->
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt | Quit -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () = 
  print_endline "\n\n\nJoDy\nWarning: Dynamic functions that use a timer may not work correctly.\nDynamic functions should be pure, ones that rely on side effects may encounter issues.";
  Lwt_main.run (main ())
