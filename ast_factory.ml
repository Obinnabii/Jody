open Ast

let make_let_defn x e =
  DLet (x, e)

let make_let_rec_defn f x e =
  DRec (f, x, e)

let make_let_dyn_defn f x e =
  DDyn (f, x, e)

let make_seq e1 e2 =
  ESeq (e1, e2)

let make_app e es =
  EApp (e, es)

let make_unop uop e =
  EUnop (uop, e)

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_if e1 e2 e3 =
  EIf (e1, e2, e3)

let make_if_partial e1 e2 =
  failwith "Illegal"

let make_let x e1 e2 =
  ELet (x, e1, e2)

let make_let_rec f x e1 e2 =
  ERec(f, x, e1, e2)

let make_let_dyn f x e1 e2 =
  EDyn(f, x, e1, e2)

let make_try e1 x e2 =
  failwith "Illegal"

let make_try_finally e1 x e2 e3 =
  failwith "Illegal"

let make_throw e =
  failwith "Illegal"

let make_ref e =
  failwith "Illegal"

let make_fun xs e =
  EFun (xs, e)

let make_while e1 e2 =
  failwith "Illegal"

let make_delete_field e1 e2 =
  failwith "Illegal"

let make_var x =
  EVar (x)

let make_int s =
  EInt (int_of_string s) 

let make_string s =
  failwith "Illegal"

let make_bool b =
  EBool (b)

let make_undefined () =
  failwith "Illegal"

let make_object fields =
  failwith "Illegal"

let make_get_field e1 e2 =
  failwith "Illegal"
