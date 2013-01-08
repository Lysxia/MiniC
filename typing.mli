(** Mini-C compiler **)
(* Typed trees *)

exception E of string

module Imap : Map.S with type key=int

type tident = int
type tname = string

type tt =
  | V | I | C
  | S of tident
  | U of tident
  | P of int*tt | Null

type 'a typed = { tdesc:'a ; t:tt }


type tbinop = Ast.binop

type tunop = Ast.unop


type texpr = tedesc typed

and tedesc =
  | TCi     of Int32.t
  | TCs     of string
  | TLoc    of tident
  | TGlo    of tname
  | TDot    of texpr*tident
  | TAssign of texpr*texpr
  | TCall   of tname*texpr list
  | TUnop   of tunop*texpr
  | TBinop  of tbinop*texpr*texpr
  | TSizeof of tt


type tvdec = tt*tname

type tinstr =
  | TNop
  | TExpr   of texpr
  | TIf     of texpr*tinstr*tinstr
  | TWhile  of texpr*tinstr
  | TFor    of texpr list*texpr*texpr list*tinstr
  | TBloc   of tinstr list
  | TReturn of texpr option

type tconstr = tt*tt array

type tfct = {
  tret    : tt;
  tfid    : tname;
  formals : int;
  locals  : tt array;
  tbody   : tinstr;
  }

type tfile = tconstr list*tfct list*tvdec list

type env

val type_expr  : Ast.expr  -> texpr
val type_instr : Ast.instr -> tinstr*env
val type_prog  : Ast.file  -> tfile

val reset : unit -> unit
