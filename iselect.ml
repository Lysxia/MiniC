(** Mini-C Compiler **)
(* Li-yao Xia *)

open Typing

type munop = Neg
  | Addi of Int32.t | Divi of Int32.t | Muli of Int32.i
  | Subi of Int32.t | Remi of Int32.t

type mbinop =
  | Add | Div | Mul | Sub | Rem | Seq

type expr =
  | Econst of Int32.t
  | Emunop of munop*expr
  | Embinop of mbinop*expr*expr
  | Eand of expr*expr
  | Eor of expr*expr

(********************************)

let int16 n = -32767<n && n<32768

let sizeof = function
  | _ -> assert false

let rec mk_add e1 e2 = match e1,e2 with
  | Econst m, Econst n ->
      Econst (Int32.add m n)
  | e, Econst Int32.zero
  | Econst Int32.zero, e -> e
  | Emunop (Addi m, e), Econst n
  | Econst n, Emunop (Addi m, e) ->
      mk_add (Econst (Int32.add m n)) e
  | Emunop (Subi m, e), Econst n
  | Econst n, Emunop (Subi m, e) ->
      mk_add (Econst (Int32.sub n m)) e
  | e1, Emunop (Neg, e2)
  | Emunop (Neg, e2), Econst e1 ->
      mk_sub e1 e2 (* The evaluation order of binop is not specified *)
  | e,Econst n | Econst n,e when int16 n ->
      Emunop (Addi n, e)
  | _ -> Embinop (Add, e1, e2)

and mk_sub e1 e2 = match e1,e2 with
  | Econst m, Econst n ->
      Econst (Int32.sub m n)
  | e, Econst Int32.zero -> e
  | Econst Int32.zero, e ->
      mk_neg e
  | e1,Emunop (Neg, e2) ->
      mk_add e1 e2
  | e, Econst n when int16 n ->
     Emunop (Subi n, e)
  | Econst n, e when int16 n ->
     Emunop (Neg, (Emunop (Subi n, e)))
  | _ -> Embinop (Sub, e1, e2)
  
and mk_neg = function
  | Econst n -> Econst (Int32.neg n)
  | Emunop (Neg, e) -> e
  | Ebinop (Sub, e1, e2) -> mk_sub e2 e1
  | e -> Emunop (Neg, e)

let rec mk_mul e1 e2 = match e1,e2 with
  | _ -> assert false

and mk_div e1 e2 = match e1,e2 with
  | _ -> assert false

