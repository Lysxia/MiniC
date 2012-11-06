(** Mini-C typing **)
(* Li-yao Xia *)

(* I refer to gcc whenever the paper is not precise enough
 * or specifications bother me and doing like gcc is lazier *)

open Ast

exception E of string

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


type tvstmt = tt*tident

type tinstr = tidesc typed

and tidesc =
  | TNop
  | TIf of texpr*tinstr*tinstr
  | TWhile of texpr*tinstr
  | TFor of texpr list*texpr*texpr list*tinstr
  | TBloc of tvstmt list*tinstr list
  | TReturn of texpr option

type tconstr = tt*tt array

type tfct = tt*tvstmt list*tvstmt list*tinstr list

type tfile =
  (int,tconstr) Hashtbl.t*tfct list*tvstmt list
(*****)

(* Identifiers are converted to numbers,
 * this is for absolute efficiency only *)

type env = {
  f : (tt*tt list) Smap.t ; (* functions *)
  su : tt Smap.t; (* defined structure/union types *)
  sm : (tt*tident) Smap.t Imap.t ; (* s/u members *)
  v : (tt*tident) Smap.t ; (* variables *)
  }

let empty = {f=Smap.empty;su=Smap.empty;sm=Imap.empty;v=Smap.empty}

let globals = ref empty

(* For error reporting only 
 * Find structure/union type identifier *)
let su_string_of_tid = Hashtbl.create 7

let rec stringtype = function
  | V -> "void"
  | I -> "int"
  | C -> "char"
  | S s -> "struct "^(Hashtbl.find su_string_of_tid s)
  | U s -> "union "^(Hashtbl.find su_string_of_tid s)
  | P (n,t) -> (stringtype t)^String.make n '*'
  | Null -> "typenull"

let error loc s =
  raise (Error.E (loc.start_p,loc.end_p,s))

let su_id = function
  | U i | S i -> i
  | _ -> invalid_arg "Not a struct nor union id"
(*****)

(* Type relationships *)

let addable = function
  | I | C | Null -> true
  | _ -> false

let compatible t1 t2 = match t1,t2 with
  | _,_ when addable t1 && addable t2 -> true
  | P (1,V), P (_,_) | P (_,_), P (1,V)
  | Null, P (_,_) | P (_,_),Null -> true
  | _,_ when t1=t2 -> true
  | _ -> false

let num t =
  compatible t I || compatible t (P (73,I))

(*****)

(* Typing *)

(* gcc does not allow struct and union
 * with the same identifier, so we don't either *)
let rec wft (* well formed type*) = function
  | Void -> V
  | Int -> I
  | Char -> C
  | Point (n,t) -> P (n,wft t)
  | Struct s ->
      begin
        try
          match Smap.find s !globals.su with
            | S i -> S i
            | _ -> raise Not_found
        with
          Not_found -> raise (E (s^
            " is not a structure identifier"))
      end
  | Union s ->
      try
        match Smap.find s !globals.su with
          | U i -> U i
          | _ -> raise Not_found
      with
        Not_found -> raise (E (s^
            " is not a union identifier"))

let rec lvalue = function
  | TId _ | TUnop (Star,_) -> true
  | TDot (e,_) -> lvalue e.tdesc
  | _ -> false

let mkt d t = { tdesc=d ; t=t }

(* Expression typing *)

(* In typeunop : case Star,
 * this is a bit more restrictive than specifications on the paper
 * Example :
 * # *(0+0) : int
 *   # 0+0 : int*
 *     # 0 : typenull
 *     # 0 : typenull
 *     # typenull === int*
 *     # typenull === int
 *     # + = '+' or '-'
 * is well typed theoretically but dereferences a NULL pointer
 * Forbidding such cases is safer,
 * I can be lazier that way because no unifying needs to be done
 * and gcc does so too
 * *)

let typedot env e i =
  try
    let t,i = Smap.find i (Imap.find (su_id e.t) env.sm) in
    mkt (TDot (e,i)) t
  with
    | Not_found -> raise (E ("\'"^(stringtype e.t)^
        "\' has no member named \'"^i^"\'"))

let typeassign e1 e2 =
  if lvalue e1.tdesc
    then begin
      if compatible e1.t e2.t
        then mkt (TAssign (e1,e2)) e1.t
        else raise (E (
          "Incompatible types when assigning to type \'"^
          (stringtype e1.t)^"\' from type \'"^
          (stringtype e2.t)^"\'"))
    end
    else raise (E
      "lvalue required as left operand of assignment")

let typecall =
  let rec matchtype i = function
    | [],[] -> ()
    | t::tt,e::et when compatible t e.t -> matchtype (i+1) tt et
    | [],_ -> raise (E "This function is applied to too many arguments")
    | _,[] -> raise (E "This function is applied to too few arguments")
    | t::_,e::_ -> raise (E
        ("Argument "^(string_of_int i)^" has type \'"^
          (stringtype e.t)^
          "\' but an expression was expected of type \'"^
          (stringtype t)^"\'"))
  in
  fun env f el ->
  try
    let rett,argt = Smap.find f env.f in
    matchtype argt el;
    mkt (TCall (

let typeunop o e =
  (**)
  assert false

(* Addition and substraction rules create a lot of cases to be checked *)
let typebinop =
  let arith_rule o (* (+,-,*,/,%,||,&&) *) e1 e2 =
    if compatible e1.t e2.t && compatible e1.t I
      then mkt (TBinop (o,e1,e2)) I
      else raise (E "operands are not numerically compatible")
  in
  let add_sub_rule o (* (+,-) *) e1 e2 =
    try arith_rule o e1 e2 with
      E _ ->
        if compatible e1.t (P (1,V)) && compatible e2.t I
          then mkt (TBinop (o,e1,e2)) (P (1,V))
          else raise (E "operands are not numerically compatible")
  in
  let add_rule o (*'+'*) e1 e2 =
    try add_sub_rule o e1 e2 with
      E _ -> add_sub_rule o e2 e1
  in
  let sub_rule o (*'-'*) e1 e2 =
    try add_sub_rule o e1 e2 with
      E _ ->
        (**)
        assert false
  in
  fun o e1 e2 -> match o with
  | Eq | Neq | Lt | Leq | Gt | Geq ->
      if compatible e1.t e2.t && num e1.t
        then mkt (TBinop (o,e1,e2)) I
        else raise (E "operands are not numerically compatible")
  | Mul | Div | Mod | And | Or ->
      arith_rule o e1 e2
  | Add -> add_rule o e1 e2
  | Sub -> sub_rule o e1 e2

(* Main expression typing function *)
let rec typeexpr env { desc=edesc ; loc=loc } =
  try
    match edesc with
      | Cint 0 -> mkt (TCi 0) Null
      | Cint i -> mkt (TCi i) I
      | Cstring s -> mkt (TCs s) (P (1,C))
      | Ident x -> let t,i = Smap.find x env.v in
          mkt (TId i) t
      | Dot (e,i) ->
          let e = typeexpr env e in
          typedot env e i
      | Assign (e1,e2) ->
          let e1 = typeexpr env e1 in
          let e2 = typeexpr env e2 in
          typeassign e1 e2
      | Call (f,elist) ->
          let el = List.map
            (typeexpr env) elist in
          typecall env f el
      | Unop (o,e) -> let e = typeexpr env e in
          typeunop o e
      | Binop (o,e1,e2) ->
          let e1 = typeexpr env e1 in
          let e2 = typeexpr env e2 in
          typebinop o e1 e2
      | Sizeof t -> (* gcc sizeof(void)=1 *)
          let t = wft t in
          if t = V
            then raise (E "type \'void\' has no size")
            else mkt (TSizeof t) I
  with
    | E s -> error loc s
(*****)

(* Instruction typing *)
(*****)

(* Program typing *)

let type_prog ast =
  assert false
(*****)

(*______\o/_______________/[___S_H_A_R_K___A_T_T_A_C_K___!___*)
