(** Mini-C typing **)
(* Li-yao Xia *)

type tident = int

type tt = V | I | C | S of ident | U of ident | P of int*tt

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
  | TReturn of (texpr option)

