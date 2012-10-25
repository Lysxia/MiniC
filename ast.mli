(** Mini-C abstract syntax **)
(** Li-yao Xia **)

type loc = Lexing.position
 
type t = Void | Int | Char
  | Struct of string | Union of string | Point of int*t

type var = string

type binop = Eq | Neq | Lt | Leq | Gt | Geq
  | Plus | Minus | Mul | Div | Mod | And | Or

and unop = Incrp | Decrp | Incrs | Decrs (* Plus-plus/Minus-minus pre/suffix *)
  | Address | Not | Uminus | Uplus | Star

and expr = { edesc:edesc ; e_start:loc ; e_end:loc }

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

type vstmt = { vdesc:vdesc ; v_start:loc ; v_end:loc }

and vdesc = t*((int*var) list)

type instr =
  | Loc of iloc
  | Instr of idesc

and iloc = { idesc:idesc ; i_start:loc ; i_end:loc }


and idesc =
  | Nop
  | Expr of expr
  | If of expr*instr*instr
  | While of expr*instr (* a for loop is a kind of while loop *)
  | Bloc of (vstmt list)*(instr list)
  | Return of (expr option)

type stmt = { sdesc:sdesc ; s_start:loc ; s_end:loc }

and sdesc =
  | Vars of vstmt
  | Typ of t*(vstmt list)
  | Fct of t*var*((t*var) list)*(vstmt list)*(instr list)

type file = stmt list

exception Error of loc * string
