(** Mini-C Compiler **)
(* Li-yao Xia *)

open Typing

module Iset = Set.Make(struct type t=int let compare=compare end)

type munop = Neg
  | Addi of Int32.t | Divi of Int32.t | Muli of Int32.t
  | Subi of Int32.t | Remi of Int32.t

type mbinop =
  | Add | Div | Mul | Sub | Rem | Seq

type expr =
  | Mconst  of Int32.t (* li *)
  | Mmove   of expr*expr (* first must be a lvalue *)
  | Munop  of munop*expr
  | Mbinop of mbinop*expr*expr
  | Mlvar   of tident (* lw or use registers *)
  | Mgvar   of string (* la *)
  | Mla     of string
  | Mlw     of expr*int (*16*)
  | Msw     of expr*int (*16*)
  | Mand    of expr*expr
  | Mor     of expr*expr
  | Mzero   of expr (* e*0 -> e; 0; if e is not "pure" *)
  | Mcall   of string*expr list

(********************************)

let tsize = Hashtbl.create 17

let data = Hashtbl.create 5

let sizeof = function
  | I -> 4
  | C -> 1
  | P _ -> 4
  | S i | U i -> Hashtbl.find tsize i
  | _ -> assert false

let rec mk_add e1 e2 = match e1,e2 with
  | Mconst m, Mconst n ->
      Mconst (Int32.add m n)
  | e, Mconst n
  | Mconst n, e when n = Int32.zero -> e
  | Mmunop (Addi m, e), Mconst n
  | Mconst n, Mmunop (Addi m, e) ->
      mk_add (Econst (Int32.add m n)) e
  | Munop (Addi m, e1), e2
  | e2, Munop (Addi m, e1) ->
      mk_add (Mconst m) (mk_add e1 e2)
  | Munop (Subi m, e), Mconst n
  | Mconst n, Munop (Subi m, e) ->
      mk_add (Econst (Int32.sub n m)) e
  | Munop (Subi m, e1), e2
  | e2, Munop (Subi m, e1) ->
      mk_sub (mk_add e1 e2) (Mconst m)
  | e1, Munop (Neg, e2)
  | Munop (Neg, e2), e1 ->
      mk_sub e1 e2 (* The evaluation order of binop is not specified *)
  | e,Mconst n | Mconst n,e ->
      Munop (Addi n, e)
  | _ -> Mbinop (Add, e1, e2)

and mk_sub e1 e2 = match e1,e2 with
  | Mconst m, Mconst n ->
      Mconst (Int32.sub m n)
  | e, Mconst n when n=Int32.zero -> e
  | Mconst n, e when n=Int32.zero ->
      mk_neg e
  | e1,Munop (Neg, e2) ->
      mk_add e1 e2
  | e, Mconst n when int16 n ->
     Munop (Subi n, e)
  | Mconst n, e when int16 n ->
     Munop (Neg, (Munop (Subi n, e)))
  | _ -> Mbinop (Sub, e1, e2)

and mk_neg = function
  | Mconst n -> Mconst (Int32.neg n)
  | Munop (Neg, e) -> e
  | Mbinop (Sub, e1, e2) -> mk_sub e2 e1
  | Munop (Muli n, e) ->
      Munop (Muli (Int32.neg n), e)
  | Munop (Divi n, e) ->
      Munop (Divi (Int32.neg n), e)
  | e -> Munop (Neg, e)

let rec pure e = match e with
  | Mconst _ | Mlvar _ | Mgvar _ -> true
  | Munop (_,e) | Mlw (e,_) | Msw (e,_) | Mzero e -> pure e
  | Mmove (_,_) | Mcall _ -> false
     (* functions can be examined for pureness *)
  | Mbinop (_,e1,e2) | Mand (e1,e2) | Mor (e1,e2) ->
      pure e1 && pure e2

let rec mk_mul e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (Int32.mul n m)
  | Mconst n, Munop (Muli m, e)
  | Munop (Muli m, e), Mconst n ->
      mk_mul (Mconst (Int32.mul m n)) e
  | Mconst n, e | e, Mconst n when n=Int32.zero ->
      if pure e
        then Mconst Int32.zero
        else Mzero e
  | Mconst n, e | e, Mconst n ->
     Munop (Muli n, e)
  | e1, e2 -> Mbinop (Mul, e1, e2)
 (* use shifts when mul a power of 2 *)
 (* e*0 -> check pure e*)

and mk_div e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (Int32.div n m)
  | e, Mconst n ->
      if n = Int32.zero
        then Printf.printf "Warning : Divide by zero";
      Munop (Divi Int32.zero, e)
  | e1,e2 -> Mbinop (Div, e1, e2)
 (* use shifts when div a power of 2 *)
 (* e/0 -> ??*)

and mk_rem e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (Int32.rem n m)
  | e, Mconst n -> Munop (Remi n, e)
  | e1,e2 -> Mbinop (Rem, e1, e2)
  (* use masks when mod a power of 2 (?) *)

let mk_assign e1 e2 = assert false

let mk_string =
  let free = ref 0 in
  fun s -> incr free;
    Hashtbl.add data ("string"^string_of_int !free);
    Mla ("_string"^string_of_int !free)

let select_expr env genv {tdesc=e ; t=tt} = match e with
  | TCi n -> Mconst n
  | TCs s -> mk_string s
  | TId i ->
      if Iset.find i env
        then Mlvar i
        else Mgvar genv.(i) (* Should probably correct typing *)
  | TDot (e,i) -> assert false (* Not implemented *)
  | TAssign (e1,e2) ->
    mk_assign (select_expr env genv e1) (select_expr env genv e2)
  | TCall (f,l) -> Mcall (f,List.map (select_expr env genv) l)
  | 

