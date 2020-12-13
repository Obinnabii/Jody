open Ast

exception RunTimeError of string

type time = Reset | Start of float

type value = 
  | VInt of int
  | VBool of bool
  | VClosure of id list * expr * env
  | VRecClosure of id list * expr * env ref
  | VDynClosure of id list * expr * env ref * id

and env = (id * value) list (** Sigma *)

let rec val_compare (v1:value list) (v2:value list) = 
  match v1, v2 with 
  | h1::t1, h2::t2 -> begin 
      match h1, h2 with 
      | VInt n1, VInt n2 -> begin 
          let c = compare n1 n2 in 
          if c == 0 then val_compare t1 t2 else c 
        end
      | VBool b1, VBool b2 ->begin  
          let c = compare b1 b2 in 
          if c == 0 then val_compare t1 t2 else c 
        end
      | VBool b1, VInt n1 -> begin
          let c = compare (if b1 then 1 else 0) n1 in 
          if c == 0 then val_compare t1 t2 else c 
        end
      | VInt n1, VBool b1 -> begin
          let c = compare n1 (if b1 then 1 else 0) in 
          if c == 0 then val_compare t1 t2 else c 
        end
      | _ -> failwith "Dynamic: unsupported type for dynamic function"
    end 
  | [], [] -> 0
  | _ -> failwith "Dynamic: Wrong input size"


module Cache = Map.Make(struct type t = value list;; let compare = val_compare end)

type state = {time: time; 
              dyn_funs: (id * value Cache.t ref) list; 
              low_mem_mode: bool; 
              display: bool} (** The dynamic map *)

let low_mem_capacity = 1000

let failwith x = raise (RunTimeError(x))

let initial_env = []

let initial_state = {time = Reset; 
                     dyn_funs = []; 
                     low_mem_mode = false;
                     display = false }

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> Bool.to_string b
  | VClosure _ -> "<fun>"
  | VRecClosure _ -> "<rec fun>"
  | VDynClosure _ -> "<dyn fun>"

let string_of_env =
  List.fold_left 
    (fun acc (id, v) -> 
       acc ^ id ^ " -> " ^ string_of_value v ^ ";\n") "" 

let string_of_state st =
  failwith "Unimplemented state"

let get_val = function
  | (v, _) -> v

(** [to_bool v] is false is [v] is undefined, false, "", 0. True, otherwise. *)
let to_bool v = 
  match v with 
  | VBool v -> VBool v 
  | VInt 0 -> VBool false 
  | VInt _ -> VBool true
  | _ -> failwith "Casting: you tried to cast a function into a bool"

(** [to_int v] is undefined if [v] is undefined, location, closure, extern, or 
    object, i if [v] is the integer i, 1 if [v] is true, 0 if [v] is false, i if 
    [v] is a string s and i = int_of_string s, undefined if [v] is a string s and 
    int_of_string s raises Failure. *)
let to_int v = 
  match v with 
  | VInt i -> VInt i
  | VBool true -> VInt 1 
  | VBool false -> VInt 0
  | _ -> failwith "Casting: you tried to cast a function into an int"

let rec eval_expr (e, env, st) = 
  match e with
  | EApp (e, args) -> eval_app e args st env
  | EBinop (b, e1, e2) -> eval_binop b e1 e2 env st
  | EBool(b) -> (VBool(b), st)
  | EFun (xs, e) -> eval_fun xs e env st
  | EIf (e1, e2, e3) -> eval_if e1 e2 e3 env st
  | EInt(n) -> (VInt(n), st)
  | ELet (x, e1, e2) -> eval_let x e1 e2 env st
  | ELetRec (name, xs, e1, e2) -> eval_let_rec name xs e1 e2 env st
  | ELetDyn (name, xs, e1, e2) -> eval_let_dyn name xs e1 e2 env st
  | ESeq (e1, e2) -> eval_seq e1 e2 env st 
  | EStart -> eval_start st
  | EStop -> eval_stop st
  | EUnop (u, e1) -> eval_unop u e1 env st
  | EVar(x) -> eval_var x env st
(* | _ -> failwith "unimplemented expr" *)

and eval_if e1 e2 e3 env st =
  (* get the values *)
  let v1, st1 = (e1, env, st) |> eval_expr in
  match v1 |> to_bool, st1 with
  | VBool b, st' -> eval_expr ((if b then e2 else e3), env, st')
  | _ -> failwith "If: e1 not a bool"

and eval_seq e1 e2 env st =
  (* get the values *)
  let _, st' = (e1, env, st) |> eval_expr in
  eval_expr (e2, env, st')

and eval_var x env st =
  match List.assoc_opt x env with
  | Some value -> (value, st)
  | None -> failwith "Variable: free variable error"

and eval_unop uop e1 env st =
  let v1 = eval_expr (e1, env, st) |> get_val in
  match uop with
  | UopNot -> begin
      match (v1 |> to_bool) with
      | VBool x -> (VBool (not x), st)
      | _ -> failwith "Not: doesn't type check"
    end
  | UopMinus -> begin match (v1 |> to_int) with
      | VInt i -> (VInt (~- i), st)
      | _ -> failwith "Minus: doesn't type check"
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
  | _ -> failwith "Math Binop: doesn't type check"

and simple_bool_helper e1 e2 env st op =
  let v1 = eval_expr(e1, env, st) |> get_val |> to_bool in
  let v2 = eval_expr(e2, env, st) |> get_val |> to_bool in
  match v1, v2 with
  | VBool b1, VBool b2 -> (VBool ((op) b1 b2), st)
  | _ -> failwith "Boolean Binop: doesn't type check"

and simple_cmp_helper e1 e2 env st op =
  let v1 = eval_expr(e1, env, st) |> get_val |> to_int in
  let v2 = eval_expr(e2, env, st) |> get_val |> to_int in
  match v1, v2 with
  | VInt i1, VInt i2 -> (VBool ((op) i1 i2), st)
  | _ -> failwith "Comparison Binop: doesn't type check"

(** [def_let x (r,st) env] adds the binding of [x] and the value of the expresion
    [r] in [st] to [env] and returns the new environment.  *)
and def_let x (r,st) env =
  let new_env = (x, get_val (r,st)) :: (env) in
  (r, new_env ,st)

and eval_let x e1 e2 env st =
  let v, st' = eval_expr (e1, env, st) in
  match v with
  | VInt _ | VBool _  | VClosure _ -> eval_expr (e2, (x, v)::env, st')
  | _ -> failwith "Definition: cannot assign Recursive/Dynamic functions in this manner"

and eval_let_rec name xs e1 e2 env st =
  let dummy_env = ref [] in
  let v1 = VRecClosure(xs, e1, dummy_env) in
  let env' = (name, v1 )::env in
  dummy_env := env';
  eval_expr (e2, env', st)

and eval_let_dyn name xs e1 e2 env st =
  let dummy_env = ref [] in
  let v1 = VDynClosure(xs, e1, dummy_env, name) in
  let env' = (name, v1 )::env in
  dummy_env := env';
  let empty = ref Cache.empty in
  let st' = {st with dyn_funs=(name, empty)::st.dyn_funs} in
  eval_expr (e2, env', st')

and eval_stop st =
  match st.time with
  | Reset -> failwith "Timer: stop called before start"
  | Start t -> (VInt(((Sys.time() -. t) *. 1000.0) |> Int.of_float),
                {st with time = Reset})

and eval_start st =
  let t = Sys.time() in
  (VInt(t *. 1000.0 |> Int.of_float), {st with time = Start t})

and eval_fun xs e env st = VClosure(xs, e, env), st


and is_closure = function 
  | VClosure _ 
  | VRecClosure _ 
  | VDynClosure _ -> true
  | _ -> false

and eval_app e args st env =

  let rec unfold_params args xs env1 st1 e = match args, xs with
    | a :: rgs, x :: s -> let v, st1' = eval_expr (a, env, st1) in
      unfold_params rgs s ((x,v)::env1) st1' e
    | [], [] -> eval_expr (e, env1, st1)
    | _, _ -> failwith "Application: wrong number of arguments"
  in

  let rec unfold_params_dyn args xs env1 st1 e vals first_order name = begin 
    match args, xs, first_order with
    | a :: rgs, x :: s, _ -> begin 
        match eval_expr (a, env, st1) with
        | v, st1' when is_closure v -> begin 
            print_endline "\nWarining: first order functions are not memoized";
            unfold_params_dyn rgs s ((x,v)::env1) st1' e (v::vals) true name 
          end
        | v, st1' -> unfold_params_dyn rgs s ((x,v)::env1) st1' e (v::vals) first_order name
      end
    | [], [], true -> eval_expr (e, env1, st1)
    | [], [], false -> cache_lookup name vals e env1 st1
    | _ -> failwith "Application: wrong number of arguments"
  end
  in

  match eval_expr (e, env, st) with
  | VClosure (xs, e_body, env_cl), st' -> unfold_params args xs env_cl st' e_body
  | VRecClosure (xs, e_body, env_rec), st' -> unfold_params args xs !env_rec st' e_body
  | VDynClosure (xs, e_body, env_dyn, name), st' -> unfold_params_dyn args xs !env_dyn st' e_body [] false name
  | _ -> failwith "Application: not a function"

and is_mem display mem_str vals = 
  let list_to_string acc v= acc^(string_of_value v) in
  if display then 
    print_endline (List.fold_left list_to_string mem_str vals)

and cache_lookup name vals e env st = 
  match List.find_opt (fun (x, _) -> x == name) st.dyn_funs with 
  | Some (_, cache) -> begin 
      match Cache.(!cache |> find_opt vals) with
      | Some v -> begin
          is_mem st.display "\nMEMOIZED ->" vals;
          v, st
        end
      | None -> begin 
          let v, st' = eval_expr (e, env, st) in
          is_mem st.display "\nNOT MEMOIZED ->" vals;
          cache := cache_add cache vals v st.low_mem_mode;
          v, st
        end
    end
  | None -> failwith "dynamic function not initialized properly, not in cache"

and cache_add cache vals v low_mem_mode = 
  if not low_mem_mode 
  then Cache.(!cache |> add vals v)
  else begin
    if Cache.(cardinal !cache <= low_mem_capacity) 
    then Cache.(!cache |> add vals v)
    else 
      let removed_key = Cache.(choose !cache |> fst) in
      Cache.(
        !cache
        |> remove removed_key
        |> add vals v
      )
  end 

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)

let eval_defn (d, env, st) =
  match d with
  | DLet (x, e) -> let e1 = (e, env, st) |> eval_expr in begin
      match get_val e1 with
      | VInt _ | VBool _  | VClosure _ -> def_let x e1 env
      | _ -> failwith "Definition: cannot assign Recursive/Dynamic functions in this manner"
    end
  | DLetRec (name, xs, e) -> begin
      let dummy_env = ref [] in
      let e' = VRecClosure(xs, e, dummy_env) in
      let env' = (name, e')::env in
      dummy_env := env';
      def_let name (e',st) env'
    end
  | DLetDyn (name, xs, e) -> begin
      let dummy_env = ref [] in
      let e' = VDynClosure(xs, e, dummy_env, name) in
      let env' = (name, e')::env in
      let empty = ref Cache.empty in
      let st' = {st with dyn_funs=(name, empty)::st.dyn_funs} in
      dummy_env := env';
      def_let name (e',st') env'
    end

let eval_phrase (p, env, st) =
  try
    match p with
    | Expr e -> let r, st' =  eval_expr (e, env, st) in (r, env, st')
    | Defn d -> eval_defn (d, env, st)
  with
  | RunTimeError _ as exn -> raise exn
  | e -> failwith ((e |> Printexc.to_string))


