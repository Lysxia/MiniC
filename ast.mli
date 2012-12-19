(** Mini-C abstract syntax **)
(** Li-yao Xia **)

type location = {
  start_p:Lexing.position ;
  end_p:Lexing.position }

type 'a loc = {
  desc:'a ;
  loc:location }

type ident = string

type t =
  | Void | Int | Char
  | Struct of ident
  | Union of ident
  | Point of int*t

type binop =
  | Eq | Neq | Lt | Leq | Gt | Geq
  | Add | Sub | Mul | Div | Mod | And | Or

type unop =
  | Incrp | Decrp | Incrs | Decrs (* Plus-plus/Minus-minus pre/suffix *)
  | Address | Not | Uminus | Uplus | Star

type expr = edesc loc

and edesc =
  | Cint    of Int32.t
  | Cstring of string
  | Ident   of ident
  | Dot     of expr*ident
  | Assign  of expr*expr
  | Call    of ident*expr list
  | Unop    of unop*expr
  | Binop   of binop*expr*expr
  | Sizeof  of t


type vdec = vdesc loc

and vdesc = t*ident


type instr =
  | Instr  of idesc loc
  | Expr   of expr

and idesc =
  | Nop
  | If     of expr*instr*instr
  | While  of expr*instr
  | For    of expr list*expr option*expr list*instr
  | Bloc   of vdec list*instr list
  | Return of expr option

type fct = {
  ret:t;
  fid:ident;
  arg:vdec list;
  locv:vdec list;
  body:instr list;
  }

type dec = 
  | Dec of ddesc loc
  | V   of vdec

and ddesc =
  | Typ of t*vdec list
  | Fct of fct

type file = dec list loc

