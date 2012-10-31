(** Mini-C typing **)
(* Typed trees *)

module Imap : Map.S with type key=int

type tident = int

type zero = Zero | Ii | Null

type tt = V | I | C | S of tident | U of tident
  | P of int*tt | Z of zero ref

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
  | TCall of tident*(texpr list)
  | TUnop of tunop*texpr
  | TBinop of tbinop*texpr*texpr
  | TSizeof of tt


type tvstmt = tt*tident

type tinstr = tidesc typed

and tidesc =
  | TNop
  | TIf of texpr*tinstr*tinstr
  | TWhile of texpr*tinstr
  | TFor of (texpr list)*texpr*(texpr list)*tinstr
  | TBloc of (tvstmt list)*(tinstr list)
  | TReturn of (texpr option)

type tconstr = tt*(tvstmt list)

type tfct = tt*(tvstmt list)*(tvstmt list)*(tinstr list)

type tfile = ((tident,tconstr) Hashtbl.t)*(tfct list)*(tvstmt list)

val type_prog : Ast.file -> tfile
