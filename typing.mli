(** Mini-C typing **)
(* Typed trees *)

module Imap : Map.S with type key=int

type tident = int

type tt = V | I | C
  | S of tident
  | U of tident
  | P of int*tt | Null

type 'a typed = { tdesc:'a ; t:tt }


type tbinop = Ast.binop

type tunop = Ast.unop


type texpr = tedesc typed

and tedesc =
  | TCi of int
  | TCs of string
  | TId of tident
  | TDot of texpr*tident
  | TAssign of texpr*texpr
  | TCall of tident*texpr list
  | TUnop of tunop*texpr
  | TBinop of tbinop*texpr*texpr
  | TSizeof of tt


type tvdec = tt*tident

type tinstr =
  | TNop
  | TExpr of texpr
  | TIf of texpr*tinstr*tinstr
  | TWhile of texpr*tinstr
  | TFor of texpr list*texpr*texpr list*tinstr
  | TBloc of tvdec list*tinstr list
  | TReturn of texpr option

type tconstr = tt*tt array

type tfct = tt*tident*tvdec list*tinstr

type tfile = tconstr list*tfct list*tvdec list

(*
val type_expr : Ast.expr -> texpr
val type_instr : Ast.instr -> tinstr
*)

val type_prog : Ast.file -> tfile
