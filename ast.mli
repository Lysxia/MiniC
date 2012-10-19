(** Mini-C abstract syntax **)
(** Li-yao Xia **)

type loc = {
  lnum : int ;
  cnum1 : int ;
  cnum2 : int ;
}

type t = Void | Int | Char
  | Struct of string | Union of string | Point of t


type var = Name of string | Pnt of int*string

type binop = Eq | Neq | Lt | Leq | Gt | Geq
  | Plus | Minus | Mul | Div | Mod | And | Or

and unop = Incrp | Decrp | Incrs | Decrs (* Plus-plus/Minus-minus pre/suffix *)
  | Address | Not | Uminus | UPlus | Star

and expr = { edesc:edesc ; eloc:loc }

and edesc =
  | Cint of int
  | Cstring of string
  | Ident of string
  | Dot of expr*string
  | Assign of expr*expr
  | Call of string*(expr list)
  | Unop of unop*expr
  | Binop of binop*expr*expr
  | Sizeof of t*int


type instr = { idesc:idesc ; iloc:loc }

and idesc =
  | Expr of expr
  | If of expr*instr*instr
  | While of expr*instr
  | For of expr*(expr option)*expr*instr
  | Bloc of (stmt list)*(instr list)
  | Return of (expr option)

and stmt =
  | Vars of t*var
  | Typ of t*(stmt list)
  | Fct of t*var*((t*var) list)*(stmt list)*(instr list)

type file = stmt list

exception Error of loc * string
