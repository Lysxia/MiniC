(** Mini-C Compiler **)
(* Li-yao Xia *)

(* Variables are stored in pseudo-registers *)
(* instruction 'la' will force a variable to be on the stack *)

open Typing

module Iset = Set.Make(struct type t=int let compare=compare end)

type munop =
  | Neg
  | Addi of Int32.t | Muli of Int32.t | Subi of Int32.t

type mbinop =
  | Add | Div | Mul | Sub | Rem
  | Seq | Sge | Sgt | Sle | Sne
  (* Basic instr are slt and slti, 
   * which were not extended as pseudo-instr.
   * We could optimize here *)

type expr =
  | Mconst  of Int32.t (* li *)
  | Mmove   of expr*expr (* first must be a lvalue *)
  | Munop   of munop*expr
  | Mbinop  of mbinop*expr*expr
  | Mloc    of tident (* lw or use registers *)
  | Mla     of string
  | Maddr   of tident
  | Mload   of int*Int32.t*expr (* for some size *)
  | Mlw     of Int32.t*expr
  | Mlb     of Int32.t*expr
  | Mstor   of int*expr*Int32.t*expr
  | Msw     of expr*Int32.t*expr
  | Msb     of expr*Int32.t*expr
  | Mand    of expr*expr
  | Mor     of expr*expr
  | Mignore of expr*expr (* e1; e2; use if e1 is not "pure" *)
  | Mcall   of string*expr list

type instr =
  | Nop
  | Expr  of expr
  | If    of expr*instr*instr
  | While of expr*instr
  | For   of expr list*expr*expr list*instr
  | Bloc  of instr list
  | Ret   of expr option

type vdec = int*string

type fct = {retsz:int ; fid:string ; argsz:int array ; body:instr list}


let tsize = Hashtbl.create 17

let data:(int*string) list ref = ref []

let sizeof = function
  | I -> 4
  | C -> 1
  | P _ -> 4
  | S i | U i -> Hashtbl.find tsize i
  | _ -> assert false

let rec mk_add e1 e2 = match e1,e2 with
  | Mconst m, Mconst n ->
      Mconst (Int32.add m n)
  | e, Mconst n
  | Mconst n, e when n = Int32.zero -> e
  | Munop (Addi m, e), Mconst n
  | Mconst n, Munop (Addi m, e) ->
      mk_add (Mconst (Int32.add m n)) e
  | Munop (Addi m, e1), e2
  | e2, Munop (Addi m, e1) ->
      mk_add (Mconst m) (mk_add e1 e2)
  | Munop (Subi m, e), Mconst n
  | Mconst n, Munop (Subi m, e) ->
      mk_add (Mconst (Int32.sub n m)) e
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
  | Mconst n, e when n=Int32.zero -> mk_neg e
  | Munop (Subi m, e), Mconst n -> mk_sub e (Mconst (Int32.add m n))
  | Mconst m, Munop (Addi n, e) -> mk_sub (Mconst (Int32.sub m n)) e
  | Munop (Addi m, e1), e2
  | e1, Munop (Subi m, e2) -> mk_add (mk_sub e1 e2) (Mconst m)
  | e1, Munop (Addi m, e2)
  | Munop (Subi m, e1), e2 -> mk_sub (mk_sub e1 e2) (Mconst m)
  | e1,Munop (Neg, e2) -> mk_add e1 e2
  | e, Mconst n -> Munop (Subi n, e)
  | Mconst n, e -> mk_neg (Munop (Subi n, e))
  | _ -> Mbinop (Sub, e1, e2)

and mk_neg = function
  | Mconst n -> Mconst (Int32.neg n)
  | Munop (Neg, e) -> e
  | Mbinop (Sub, e1, e2) -> mk_sub e2 e1
  | Munop (Muli n, e) ->
      Munop (Muli (Int32.neg n), e)
  | Mbinop (Div, Mconst n, e) ->
      Mbinop (Div, Mconst (Int32.neg n), e)
  | e -> Munop (Neg, e)

let rec pure e = match e with
  | Mconst _ | Mloc _ -> true
     (* functions can be examined for pureness *)
  | Mbinop (_,e1,e2) | Mand (e1,e2) | Mor (e1,e2) ->
      pure e1 && pure e2
  | _ -> false

let rec mk_mul e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (Int32.mul n m)
  | Mconst n, Munop (Muli m, e)
  | Munop (Muli m, e), Mconst n ->
      mk_mul (Mconst (Int32.mul m n)) e
  | Mconst n, e | e, Mconst n when n=Int32.zero ->
      if pure e
        then Mconst Int32.zero
        else Mignore (e,Mconst Int32.zero)
  | Mconst n, e | e, Mconst n ->
     Munop (Muli n, e)
  | e1, e2 -> Mbinop (Mul, e1, e2)
 (* use shifts when mul a power of 2 *)
 (* e*0 -> check pure e*)

and mk_div e1 e2 = match e1,e2 with
  | e1,(Mconst n as e2) when n=Int32.zero ->
      Printf.printf "Warning : Divide by zero";
      Mbinop (Div,e1,e2)
  | Mconst n, Mconst m when m<>Int32.zero ->
      Mconst (Int32.div n m)
  | e1,e2 -> Mbinop (Div, e1, e2)

and mk_rem e1 e2 = match e1,e2 with
  | e1,(Mconst n as e2) when n=Int32.zero ->
      Printf.printf "Warning : Divide by zero";
      Mbinop (Rem,e1,e2)
  | Mconst n, Mconst m when m<>Int32.zero ->
      Mconst (Int32.rem n m)
  | e1,e2 -> Mbinop (Rem, e1, e2)

let rec mk_seq e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if n=m
        then Mconst Int32.one
        else Mconst Int32.zero
  | (Mconst _ as e2),e1
  | e1,(Mconst _ as e2)
  | e1,e2 -> Mbinop (Seq,e1,e2)

and mk_sge e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m >= 0
        then Mconst Int32.one
        else Mconst Int32.zero
  | (Mconst _ as e2),e1
  | e1,(Mconst _ as e2)
  | e1,e2 -> Mbinop (Sge,e1,e2)

and mk_sgt e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m > 0
        then Mconst Int32.one
        else Mconst Int32.zero
  | (Mconst _ as e2),e1
  | e1,(Mconst _ as e2)
  | e1,e2 -> Mbinop (Sgt,e1,e2)

  (*
and mk_sle e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m <= 0
        then Mconst Int32.one
        else Mconst Int32.zero
  | Mconst n as e2,e1
  | e1,Mconst n as e2
  | e1,e2 -> Mbinop (Sle,e1,e2)
*)

and mk_sne e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if n=m
        then Mconst Int32.zero
        else Mconst Int32.one
  | (Mconst _ as e2),e1
  | e1,(Mconst _ as e2)
  | e1,e2 -> Mbinop (Sne,e1,e2)

let mk_bool = function
  | Mconst n ->
      Mconst (
        if n=Int32.zero
          then Int32.zero
          else Int32.one)
  | e -> mk_sne e (Mconst Int32.zero)

let mk_and e1 e2 = match e1,e2 with
  | Mconst n,e2
  | e2,Mconst n ->
      if n=Int32.zero
        then Mconst Int32.zero
        else mk_bool e2
  | e1,e2 -> Mand (e1,e2)

let mk_or e1 e2 = match e1,e2 with
  | Mconst n,e2
  | e2,Mconst n ->
      if n=Int32.one
        then Mconst Int32.one
        else mk_bool e2
  | e1,e2 -> Mor (e1,e2)

let mk_load t_size n e =
  if t_size = 4 then Mlw (n,e)
  else if t_size=1 then Mlb (n,e)
  else Mload (t_size,n,e)

(*
let mk_stor t_size e1 n e2 =
  if t_size = 4 then Msw (e,n,e)
  else if t_size = 1 then Msb (n,e)
  else Mstor (t_size,n,e)
*)

let mk_move e1 e2 = match e1,e2 with
  | e1,e2 when e1=e2 -> e2
  | Mlw (ofs,e1),e2 -> Msw (e2,ofs,e1)
  | Mlb (ofs,e1),e2 -> Msb (e2,ofs,e1)
  | Mload (sz,ofs,e1),e2 -> Mstor (sz,e2,ofs,e1)
  | e1,e2 -> Mmove (e1,e2)

let mk_not = function
  | Mconst n when n=Int32.zero -> Mconst Int32.zero
  | Mconst n -> Mconst Int32.zero
  | Mbinop (Seq,e1,e2) -> mk_sne e1 e2
  | Mbinop (Sne,e1,e2) -> mk_seq e1 e2
  | Mbinop (Sgt,e1,e2) -> mk_sge e2 e1
  | Mbinop (Sge,e1,e2) -> mk_sgt e2 e1
  | Mbinop (Sle,e1,e2) -> mk_sgt e1 e2
  | e -> mk_seq e (Mconst Int32.zero)

let mk_deref t_size = function
  | Munop (Addi n,e) -> mk_load t_size n e
  | Munop (Subi n,e) -> mk_load t_size (Int32.neg n) e
  | Maddr i -> Mloc i
  | e -> mk_load t_size Int32.zero e

let mk_la = function
  | Mlw (n,e)
  | Mlb (n,e)
  | Mload (_,n,e) -> mk_add (Mconst n) e
  | Mloc i -> Maddr i
  | _ -> assert false

let mk_string s = assert false

let mk_unop sz u x = match u with
  | Ast.Incrp -> Mmove (x,Munop (Addi Int32.one,x))
  | Ast.Decrp -> Mmove (x,Munop (Subi Int32.one,x))
  | Ast.Incrs -> mk_sub (mk_move x (mk_add x (Mconst Int32.one))) (Mconst Int32.one)
  | Ast.Decrs -> mk_add (mk_move x (mk_sub x (Mconst Int32.one))) (Mconst Int32.one)
  (* i++ ~ (i=i+1)-1*)
  | Ast.Not -> mk_not x
  | Ast.Star -> mk_deref sz x
  | Ast.Address -> mk_la x
  | Ast.Uminus -> mk_neg x
  | Ast.Uplus -> x


let mk_binop o e1 e2 = match o with
  | Ast.Eq  -> mk_seq e1 e2
  | Ast.Neq -> mk_sne e1 e2
  | Ast.Lt  -> mk_sgt e2 e1
  | Ast.Leq -> mk_sge e2 e1
  | Ast.Gt  -> mk_sgt e1 e2
  | Ast.Geq -> mk_sge e1 e2
  | Ast.Add -> mk_add e1 e2
  | Ast.Sub -> mk_sub e1 e2
  | Ast.Mul -> mk_mul e1 e2
  | Ast.Div -> mk_div e1 e2
  | Ast.Mod -> mk_rem e1 e2
  | Ast.And -> mk_and e1 e2
  | Ast.Or  -> mk_or  e1 e2

let rec isexpr {tdesc=e ; t=tt} = match e with
  | TCi n -> Mconst n
  | TLoc i -> Mloc i
  | TGlo x -> mk_load (sizeof tt) Int32.zero (Mla x)
  | TAssign (e1,e2) ->
    mk_move (isexpr e1) (isexpr e2)
  | TCall (f,l) -> Mcall (f,List.map isexpr l)
  | TUnop (u,e) -> mk_unop (sizeof tt) u (isexpr e)
  | TBinop (o,e1,e2) -> mk_binop o (isexpr e1) (isexpr e2)
  | TSizeof t -> Mconst (Int32.of_int (sizeof t))
  | TCs s -> assert false
  | TDot (e,i) -> assert false (* Not implemented *)

let rec isinstr = function
  | TNop -> Nop
  | TExpr e -> Expr (isexpr e)
  | TIf (e,i1,i2) -> If (isexpr e,isinstr i1,isinstr i2)
  | TWhile (e,i) -> While (isexpr e,isinstr i)
  | TFor (init,cond,inc,i) ->
      For (List.map isexpr init,isexpr cond,List.map isexpr inc,isinstr i)
  | TBloc i -> Bloc (List.map isinstr i)
  | TReturn None -> Ret None
  | TReturn (Some e) -> Ret (Some (isexpr e))


let isfct {
  tret=t;
  tfid=f;
  formals=argc;
  locals=lcl;
  tbody=il;
} = {
  retsz=sizeof t;
  fid=f;
  argsz=Array.map sizeof (Array.sub lcl 0 argc);
  body=List.map isinstr il;
}

let gvars vl =
  List.map (fun (t,v) -> (sizeof t,v)) vl

let isconstr (t,fields) = assert false

let file (c,f,v) =
  Hashtbl.clear tsize;
  data := [];
  (List.map isconstr c,List.map isfct f,gvars v)
