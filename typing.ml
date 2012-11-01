(** Mini-C typing **)
(* Li-yao Xia *)

open Ast

(* Type def *)
module Imap = Map.Make(
  struct
    type t=int
    let compare=compare
  end)
 
module Smap = Map.Make(String)

 
type tident = int

(* P (n,t) when n>0 which is the case for any AST built
 * with parser *)
type tt = V | I | C | S of tident | U of tident
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

type tfile =
  ((int,tconstr) Hashtbl.t)*(tfct list)*(tvstmt list)

(**)

let tbl_int_of_string = Hashtbl.create 7
let tbl_string_of_int = Hashtbl.create 7

let tbl_fct_intid = Hashtbl.create 17
let tbl_fct_strid = Hashtbl.create 17

let rec strtype = function
  | V -> "void"
  | I -> "int"
  | C -> "char"
  | S s -> "struct "^(Hashtbl.find tbl_string_of_int s)
  | U s -> "union "^(Hashtbl.find tbl_string_of_int s)
  | P (n,t) -> (strtype t)^String.make n '*'
  | Null -> "typenull"

let error loc s =
  raise (Error.E (loc.start_p,loc.end_p,s))

(* Type relationships *)

let addable = function
  | I | C | Null -> true
  | _ -> false

let compatible t1 t2 = match t1,t2 with
  | _,_ when t1=t2 -> true
  | _,_ when addable t1 && addable t2 -> true
  | P (1,V), P (_,_) | P (_,_), P (1,V)
  | Null, P (_,_) | P (_,_),Null -> true
  | _,_ -> false

let num t =
  compatible t I || compatible t (P (73,I))

(**)

(* Typing *)

(* gcc does not allow struct and union
 * with the same identifier *)
let rec wft (* well formed type*) loc = function
  | Void -> V
  | Int -> I
  | Char -> C
  | Point (n,t) -> P (n,wft loc t)
  | Struct s ->
      begin
        try
          match Hashtbl.find tbl_int_of_string s with
            | S i,_ -> S i
            | _,_ -> raise Not_found
        with
          Not_found -> error loc (s^
            " is not a structure identifier")
      end
  | Union s ->
      try
        match Hashtbl.find tbl_int_of_string s with
          | U i,_ -> U i
          | _,_ -> raise Not_found
      with
        Not_found -> error loc (s^
            " is not a union identifier")

let rec lvalue = function
  | TId _ | TUnop (Star,_) -> true
  | TDot (e,_) -> lvalue e.tdesc
  | _ -> false

let mkt d t = { tdesc=d ; t=t }

(* type__ apply to well typed expressions *)
let typedot loc e i =
  (**)
  assert false

let typeassign loc e1 e2 =
  if lvalue e1.tdesc
    then begin
      if compatible e1.t e2.t
        then mkt (TAssign (e1,e2)) e1.t
        else error loc (
          "Incompatible types when assigning to type \'"^
          (strtype e1.t)^"\' from type \'"^
          (strtype e2.t)^"\'")
    end
    else error loc
      "lvalue required as left operand of assignment"

(*  *)
let typecall loc f el =
  (**)
  assert false

let typeunop loc o e =
  (**)
  assert false

let typebinop loc =
  let arith_rule o (* (+,-,*,/,%,||,&&) *) e1 e2 =
    if compatible e1.t e2.t && compatible e1.t I
      then mkt (TBinop (o,e1,e2)) I
      else error loc "operands are not numerically compatible"
  in
  let add_sub_rule o (* (+,-) *) e1 e2 =
    try arith_rule o e1 e2 with
      Error.E _ ->
        if compatible e1.t (P (1,V)) && compatible e2.t I
          then mkt (TBinop (o,e1,e2)) (P (1,V))
          else error loc "operands are not numerically compatible"
  in
  let add_rule o (*'+'*) e1 e2 =
    try add_sub_rule o e1 e2 with
      Error.E _ -> add_sub_rule o e2 e1
  in
  let sub_rule o (*'-'*) e1 e2 =
    try add_sub_rule o e1 e2 with
      Error.E _ -> assert false(**)
  in
  fun o e1 e2 -> match o with
  | Eq | Neq | Lt | Leq | Gt | Geq ->
      if compatible e1.t e2.t && num e1.t
        then mkt (TBinop (o,e1,e2)) I
        else error loc "operands are not numerically compatible"
  | Mul | Div | Mod | And | Or ->
      arith_rule o e1 e2
  | Add -> add_rule o e1 e2
  | Sub -> sub_rule o e1 e2

let rec typeexpr env { desc=edesc ; loc=loc } =
  match edesc with
    | Cint 0 -> mkt (TCi 0) Null
    | Cint i -> mkt (TCi i) I
    | Cstring s -> mkt (TCs s) (P (1,C))
    | Ident x -> Smap.find x env
    | Dot (e,i) ->
        let e = typeexpr env e in
        typedot loc e i
    | Assign (e1,e2) ->
        let e1 = typeexpr env e1 in
        let e2 = typeexpr env e2 in
        typeassign loc e1 e2
    | Call (f,elist) ->
        let el = List.map
          (typeexpr env) elist in
        typecall loc f el
    | Unop (o,e) -> let e = typeexpr env e in
        typeunop loc o e
    | Binop (o,e1,e2) ->
        let e1 = typeexpr env e1 in
        let e2 = typeexpr env e2 in
        typebinop loc o e1 e2
    | Sizeof t -> (* gcc sizeof(void)=1 *)
        let t = wft loc t in
        if t = V
          then error loc "type \'void\' has no size"
          else mkt (TSizeof t) I

let type_prog ast =
  (*______\o/_____________/[_______*)
  assert false
