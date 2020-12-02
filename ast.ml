(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string

type unop =
  | UopMinus
  | UopNot
  | UopTypeof (* DED *)
  | UopDeref (* KILL *)

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq
  | BopOr
  | BopAnd
  | BopEqStrict   (* DED *)
  | BopNeqStrict (* DED *)
  | BopAssign (* removed *)
  | BopUpdate (* removed *)

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr = 
  | Undef
  | EInt of int
  | EBool of bool
  | EUnop of unop * expr
  | EBinop of binop * expr * expr
  | EVar of id
  | ESeq of expr * expr
  | EIf of expr * expr * expr
  | EFun of id list * expr
  | ERec of id * id list * expr * expr
  | EDyn of id * id list * expr * expr
  | EApp of expr * expr list
  | ELet of id * expr * expr
  (* | EEmpty
     | EList of expr * expr *)



(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)

type defn = DLet of id * expr | DDyn of id * id list * expr | DRec of id * id list * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
