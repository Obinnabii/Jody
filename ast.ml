(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string

type unop =
  | UopMinus
  | UopNot


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
     [expr] is the type of the AST for expressions. You may implement
     this type however you wish.  Use the example interpreters seen in
     the textbook as inspiration.
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
  (* | EEmpty
     | EList of expr * expr *)



(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)
type defn = DLet of id * expr | DLetDyn of id * id list * expr | DLetRec of id * id list * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
