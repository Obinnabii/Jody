(******************************************************************************
   Adapted from Cornell's CS3110 A6
 ******************************************************************************)

(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  look in readme.md
 ******************************************************************************)

type id = string


type unop =
  | UopMinus
  | UopNot

type location =
  | Fst
  | Snd

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
  (* | BopEqStrict   (* DED *)
     | BopNeqStrict (* DED *)
     | BopAssign (* removed *)
       | BopUpdate removed *)

(**  ****************************************************************************
     [expr] is the type of the AST for expressions. 
 ******************************************************************************)

type expr = 
  | EApp of expr * expr list
  | EBinop of binop * expr * expr
  | EBool of bool
  | EFun of id list * expr
  | EIf of expr * expr * expr
  | EInt of int
  | ELet of id * expr * expr
  | ELetDyn of id * id list * expr * expr
  | ELetRec of id * id list * expr * expr
  | ESeq of expr * expr
  | EStart
  | EStop
  | EUnop of unop * expr
  | EVar of id
  | EPair of expr * expr
  | EFirst of expr
  | ESecond of expr
  (* | EEmpty
     | EList of expr * expr *)



(******************************************************************************
   [defn] is the type of the AST for definitions. 
 ******************************************************************************)
type defn = DLet of id * expr | DLetDyn of id * id list * expr | DLetRec of id * id list * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. 
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
