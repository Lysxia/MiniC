(** Mini-C abstract syntax **)
(** Li-yao Xia **)

type location = {
  start_p:Lexing.position ;
  end_p:Lexing.position }

type 'a loc = {
  desc:'a ;
  loc:location }

type t = Void | Int | Char
  | Struct of string | Union of string | Point of int*t

type ident = string


type binop = Eq | Neq | Lt | Leq | Gt | Geq
  | Plus | Minus | Mul | Div | Mod | And | Or

type unop = Incrp | Decrp | Incrs | Decrs (* Plus-plus/Minus-minus pre/suffix *)
  | Address | Not | Uminus | Uplus | Star

type expr = edesc loc

and edesc =
  | Cint of int
  | Cstring of string
  | Ident of string
  | Dot of expr*string
  | Assign of expr*expr
  | Call of string*(expr list)
  | Unop of unop*expr
  | Binop of binop*expr*expr
  | Sizeof of t


type vstmt = vdesc loc

and vdesc = t*ident


type instr =
  | Instr of idesc loc
  | Expr of expr

and idesc =
  | Nop
  | If of expr*instr*instr
  | While of expr*instr
  | For of (expr list)*(expr option)*(expr list)*instr
  | Bloc of (vstmt list)*(instr list)
  | Return of (expr option)


type stmt = 
  | Stmt of sdesc loc
  | V of vstmt

and sdesc =
  | Typ of t*(vstmt list)
  | Fct of t*ident*(vstmt list)*(vstmt list)*(instr list)

type file = stmt list

exception Error of location * string
