(******************************************************************************
   Adapted from Cornell's CS3110 A6
 ******************************************************************************)

(* Acknowledgement:  this parser is adapted from the OCaml 4.04 parser
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1. *)

%{
open Ast
open Ast_factory

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)
%}

%token <string> INT
%token <string> ID STRING
%token TSTART TSTOP
%token PLUS MINUS TIMES DIV MOD AND OR
       LT LEQ GT GEQ EQUAL NOTEQUAL 
       NOT 
       /* TYPEOF */
%token LPAREN RPAREN SEMI DOUBLE_SEMI ARROW LBRACE RBRACE
       COLON COMMA LBRACKET RBRACKET DOT FORWARD_SLASH
       FIRST SECOND
%token TRUE FALSE UNDEFINED
%token LET IN IF THEN ELSE BEGIN END FUN
/* FINALLY  */
       WHILE DO DONE DELETE REC DYN
%token EOF

(* The entries commented out below are unnecessary, but indicate
   the correct place in the precedence table for those tokens
   should they ever become necessary. *)

(* %nonassoc IN *)
%nonassoc below_SEMI
%nonassoc SEMI
(* %nonassoc LET *)
/* %nonassoc FINALLY */
%nonassoc THEN
%nonassoc ELSE
/* %right ASSIGN UPDATE */
(* %right ARROW *)
%right OR
%right AND
%left EQUAL NOTEQUAL LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
(* %nonassoc DOT *)
(* %nonassoc BEGIN FALSE LPAREN TRUE UNDEFINED *)

%start <Ast.expr> parse_expression
%start <Ast.phrase> parse_phrase

%%

parse_expression:
  | e = seq_expr; EOF
        { e }

parse_phrase:
  | e = seq_expr; DOUBLE_SEMI?; EOF
        { Expr e }
  | d = defn; DOUBLE_SEMI?; EOF
        { Defn d }
  | DOUBLE_SEMI?; EOF
        { raise End_of_file }
	;

defn:
  | LET; x = ID; EQUAL; e = expr
        { make_let_defn x e }
  | LET; REC; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; e = expr
        { make_let_rec_defn f xs e }
  | LET; DYN; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; e = expr
        { make_let_dyn_defn f xs e }
  ;

seq_expr:
  | e = expr; %prec below_SEMI
        { e }
  | e = expr; SEMI
        { e }
  | e = expr; SEMI; s = seq_expr;
        { make_seq e s }

expr:
  | e = simple_expr
        { e }
  | e = simple_expr; es = nonempty_list(simple_expr)
        { make_app e es }
  | uop = unop; e = expr
        { make_unop uop e }
  | e1 = expr; bop = binop; e2 = expr
        { make_binop bop e1 e2 }
  /* | e1 = expr; AND; e2 = expr
		{ make_and e1 e2 }
  | e1 = expr; OR; e2 = expr
		{ make_or e1 e2 } */
  | IF; e1 = seq_expr; THEN; e2 = expr; ELSE; e3 = expr
        { make_if e1 e2 e3 }
  | IF; e1 = seq_expr; THEN; e2 = expr
        { make_if_partial e1 e2 }
  | LET; x = ID; EQUAL; e1 = expr; IN; e2 = seq_expr
		{ make_let x e1 e2 }
  | LET; REC; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; 
    e1 = expr; IN; e2 = seq_expr
		{ make_let_rec f xs e1 e2 }
  | LET; DYN; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; 
    e1 = expr; IN; e2 = seq_expr
		{ make_let_dyn f xs e1 e2 }
  | FUN; LPAREN; xs = nonempty_list(ident); RPAREN; ARROW; e = seq_expr
        { if has_dups xs
          then $syntaxerror (* duplicate argument names *)
          else make_fun xs e }
  | WHILE; e1 = seq_expr; DO; e2 = seq_expr; DONE
        { make_while e1 e2 }
  /* | DELETE e1 = simple_expr; LBRACKET; e2 = expr; RBRACKET
        { make_delete_field e1 e2 }
  | DELETE e = simple_expr; DOT; x = ident
        { make_delete_field e (make_string x) } */
	;

simple_expr:
  | x = ident
        { make_var x }
  | LPAREN; e = seq_expr; RPAREN
        { e }
  | BEGIN; e = seq_expr; END
        { e }
  | s = INT
		{ make_int s }
  | s = STRING
		{ make_string s }
  | TSTART
            { make_start () }
  | TSTOP
            { make_stop () } 
  | TRUE
		{ make_bool true }
  | FALSE
		{ make_bool false }
  | LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET
            { make_pair e1 e2}
  | e1 = simple_expr; DOT; loc = location
        { make_get_location e1 loc }
  /* Should this be an expr */


  /* | UNDEFINED
		{ make_undefined () }
  | LBRACE; fields = separated_list(COMMA, field_bind); RBRACE
        { if fields |> List.map fst |> has_dups
          then $syntaxerror (* duplicate fields *)
          else make_object fields } 
  | e1 = simple_expr; LBRACKET e2 = expr; RBRACKET 
        { make_get_field e1 e2 } */

field_bind:
  | f = STRING; COLON; e = expr
        { (f, e) }

ident:
  | x = ID
        { x }

%inline location:
  | FIRST { Fst }
  | SECOND { Snd }

%inline unop:
  | MINUS { UopMinus }
  | NOT { UopNot }

%inline binop:
  | PLUS { BopPlus }
  | MINUS { BopMinus }
  | TIMES { BopTimes }
  | DIV { BopDiv }
  | MOD { BopMod }
  | LT { BopLt }
  | LEQ { BopLeq }
  | GT { BopGt }
  | GEQ { BopGeq }
  | EQUAL { BopEq }
  | NOTEQUAL { BopNeq }
  | OR {BopOr}
  | AND {BopAnd}
  ;
