(** Mini-C Compiler **)
(* Li-yao Xia *)

(* Variables are stored in pseudo-registers *)
(* instruction 'la' will force a variable to be on the stack *)

open Typing
open Int32
(* This module is intensively used since we do a lot of evaluation *)

module Iset = Set.Make(struct type t=int let compare=Pervasives.compare end)

type munop =
  | Neg
  | Move of int
  | Divi of t (* WRITTEN div IN MIPS *)
  | Remi of t
  | Addi of t | Muli of t | Subi of t
  | Slti of t | Seqi of t | Snei of t | Sgti of t
  | Andi of t
  | Sll of int

type mbinop =
  | Add | Div | Mul | Sub | Rem | Seq | Sne | Slt | Sle
  (* slt, sltu, sltiu, slti are basic instructions,
   * others are pseudo-instruction
   * (and seq is not optimally translated)
   * we simplified the set of bool operators *)
  (* sltu $1, $zero, $2 is $1=($2!=0) also $1=!!$2 *)

(* unsigned addu and subu do not overflow...
 * those are what we will really use *)

type expr =
  | Mconst  of t (* li *)
  | Munop   of munop*expr
  | Mbinop  of mbinop*expr*expr
  | Mloc    of tident (* lw or use registers *)
  | Mla     of string
  | Maddr   of tident
  | Mload   of int*t*expr (* for some size *)
  | Mlw     of t*expr
  | Mlb     of t*expr
  | Mstor   of int*expr*t*expr
  | Msw     of expr*t*expr
  | Msb     of expr*t*expr
  | Mand    of expr*expr
  | Mor     of expr*expr
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

type fct = {
  retsz:int;
  fid:string;
  formals:int;
  locals:int;
  locsz:int array;
  body:instr list
}

let byte = of_int 255

(**********************************************)

(* returns true and k if x=2^k *)
let log2 x =
  let rec log2 x k =
    if x=one then true,k
    else if logand one x = one then false,k
    else log2 (shift_right_logical x 1) (k+1)
  in
  if compare x zero > 0 then log2 x 0 else false,-1

let constr = Hashtbl.create 17

let data:(int*string) list ref = ref []

let aligned = function
  | C -> false
  | I | P _ -> true
  | S i | U i -> let _,al,_ = Hashtbl.find constr i in al
  | _ -> false

let sizeof = function
  | I -> 4
  | C -> 1
  | P _ -> 4
  | S i | U i -> let sz,_,_ = Hashtbl.find constr i in sz
  | V -> 0
  | Null -> assert false

let ofs i j =
  let _,_,fields = Hashtbl.find constr i in
  fields.(j)

(* Return a couple (b:bool,e:expr) where
 * - b=true if e is necessarily pure
 * - else e may not be pure *)
(* We could optimize out useless operations here,
 * but it will be done more efficiently in the next step *)
let rec pure e = match e with
  | Mbinop (_,e1,e2)
  | Mand (e1,e2)
  | Mor (e1,e2) -> pure e1 && pure e2
  | Mconst _ | Mloc _ | Mla _ | Maddr _ -> true
  | Mlw _ | Mlb _ | Mload _ | Msw _ | Msb _ | Mstor _
  | Munop (Move _,_) -> false
  | Munop (_,e) -> pure e
  | Mcall _ -> false
  (* Functions could be examined for pureness *)

(* We use the fact that the evaluation order of operands
 * of a binary operator is unspecified *)
let rec mk_add e1 e2 = match e1,e2 with
  | Mconst m, Mconst n          -> Mconst (add m n)
  | e, Mconst n
  | Mconst n, e when n = zero -> e
  | Munop (Addi m, e), Mconst n
  | Mconst n, Munop (Addi m, e) -> mk_add (Mconst (add m n)) e
  | Munop (Addi m, e1), e2
  | e2, Munop (Addi m, e1)      -> mk_add (Mconst m) (mk_add e1 e2)
  | Munop (Subi m, e), Mconst n
  | Mconst n, Munop (Subi m, e) -> mk_add (Mconst (sub n m)) e
  | Munop (Subi m, e1), e2
  | e2, Munop (Subi m, e1)      -> mk_sub (mk_add e1 e2) (Mconst m)
  | e1, Munop (Neg, e2)
  | Munop (Neg, e2), e1         -> mk_sub e1 e2
  | Munop (Muli m, e1), Munop (Muli n, e2) when m=n ->
      mk_mul (Mconst m) (mk_add e1 e2)
  | Munop (Sll k, e1), Munop (Sll l, e2) when k=l ->
      mk_mul (Mconst (shift_left one k)) (mk_add e1 e2)
  (* This doesnt work for div because strictly speaking
   * div isn't a division *)
  | Mbinop (Sub, Mconst m, e), Mconst n
  | Mconst n, Mbinop (Sub, Mconst m, e) ->
      Mbinop (Sub, Mconst (add m n), e)
  | Mbinop (Sub, Mconst m, e1), e2
  | e2, Mbinop (Sub, Mconst m, e1) ->
      mk_add (Mconst m) (mk_sub e2 e1)
  | e,Mconst n | Mconst n,e -> Munop (Addi n, e)
  | _ -> Mbinop (Add, e1, e2) 

and mk_sub e1 e2 = match e1,e2 with
  | Mconst m, Mconst n -> Mconst (sub m n)
  | e, Mconst n when n=zero -> e
  | Mconst n, e when n=zero -> mk_neg e
  | Munop (Subi m, e), Mconst n -> mk_sub e (Mconst (add m n))
  | Mconst n, Munop (Subi m, e) -> mk_sub (Mconst (add m n)) e
  | Munop (Addi n, e), Mconst m -> mk_sub e (Mconst (sub m n))
  | Mconst m, Munop (Addi n, e) -> mk_sub (Mconst (sub m n)) e
  | Munop (Addi m, e1), e2
  | e1, Munop (Subi m, e2) -> mk_add (mk_sub e1 e2) (Mconst m)
  | e1, Munop (Addi m, e2)
  | Munop (Subi m, e1), e2 -> mk_sub (mk_sub e1 e2) (Mconst m)
  | Munop (Muli m, e1), Munop (Muli n, e2) when m=n ->
      mk_mul (Mconst m) (mk_sub e1 e2)
  | Mbinop (Sub, Mconst m, e1), e2 ->
      mk_sub (Mconst m) (mk_add e1 e2)
  | e1, Mbinop (Sub, Mconst m, e2) ->
      mk_sub (mk_add e1 e2) (Mconst m)
  | e1,Munop (Neg, e2) -> mk_add e1 e2
  | Munop (Neg, e1), e2 -> mk_neg (mk_add e1 e2)
  | e, Mconst n -> Munop (Subi n, e)
  | _ -> Mbinop (Sub, e1, e2)

and mk_neg = function
  | Mconst n -> Mconst (neg n)
  | Munop (Neg, e) -> e
  | Mbinop (Sub, e1, e2)      -> mk_sub e2 e1
  | Munop (Muli n, e)         -> Munop (Muli (neg n), e)
  | Mbinop (Div, Mconst n, e) -> Mbinop (Div, Mconst (neg n), e)
  | e -> Munop (Neg, e)

and mk_mul e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (mul n m)
  | Mconst n, Munop (Muli m, e)
  | Munop (Muli m, e), Mconst n -> mk_mul (Mconst (mul m n)) e
  | Mconst n, Munop (Sll k, e)
  | Munop (Sll k, e), Mconst n ->
      mk_mul (Mconst (mul (shift_left one k) n)) e
  | Munop (Addi m, e1), (Mconst n as e2)
  | (Mconst n as e2), Munop (Addi m, e1) ->
      mk_add (mk_mul e1 e2) (Mconst (mul m n))
  | Munop (Subi m, e1), (Mconst n as e2)
  | (Mconst n as e2), Munop (Subi m, e1) ->
      mk_sub (mk_mul e1 e2) (Mconst (mul m n))
  | e2,Munop (Neg,e1) -> mk_neg (mk_mul e1 e2)
  | Mconst n, e | e, Mconst n ->
      if n=zero && pure e
        then Mconst zero
        else let p,k = log2 n in
          if p then Munop (Sll k, e) else Munop (Muli n, e)
  | Munop (Neg,e1),e2
  | e1, e2 -> Mbinop (Mul, e1, e2)
 (* use shifts when mul a power of 2 *)
 (* e*0 -> check pure e*)

and mk_div e1 e2 = match e1,e2 with
  | Mconst n, Mconst m when m<>zero ->
      Mconst (div n m)
  | e, Mconst n ->
      if n=zero
        then Printf.printf "Warning : Divide by zero";
      Munop (Divi n,e)
  | e1,e2 -> Mbinop (Div, e1, e2)

and mk_rem e1 e2 = match e1,e2 with
  | Mconst n, Mconst m when m<>zero ->
      Mconst (rem n m)
  | e1,Mconst n ->
      if n=zero
        then Printf.printf "Warning : Divide by zero";
      Munop (Remi n,e1)
  | e1,e2 -> Mbinop (Rem, e1, e2)

(* bool op *)
let rec mk_not = function
  | Mconst n ->
      if n=zero
        then Mconst one
        else Mconst zero
  | Munop (Neg,e) -> mk_not e
  | Munop (Seqi n,e) -> mk_sne (Mconst n) e
  | Munop (Snei n,e) -> mk_seq (Mconst n) e
  | Munop (Slti n,e) -> mk_sle (Mconst n) e
  | Mbinop (Seq,e1,e2) -> mk_sne e1 e2
  | Mbinop (Sne,e1,e2) -> mk_seq e1 e2
  | Mbinop (Slt,e1,e2) -> mk_sle e2 e1
  | Mbinop (Sle,e1,e2) -> mk_slt e2 e1
  | e -> Munop (Seqi zero, e)

and mk_bool e = mk_not (mk_not e)

(* Guarantee left to right evaluation *)
and mk_and e1 e2 = match e1,e2 with
  | Mconst n,e2 ->
      if n=zero
        then Mconst zero
        else mk_bool e2
  | e1,e2 -> Mand (e1,e2)

and mk_or e1 e2 = match e1,e2 with
  | Mconst n,e2 ->
      if n=zero
        then mk_bool e2
        else Mconst one
   | e1,e2 -> Mor (e1,e2)

and mk_seq e1 e2 = match mk_sub e1 e2 with
  | Mconst m -> if m=zero then Mconst one else Mconst zero
  | Munop (Subi m, e) -> Munop (Seqi m,e)
  | Munop (Addi m, e) -> Munop (Seqi (neg m),e)
  | Munop (Neg, e) -> mk_seq e2 e1
  | Mbinop (Add,_,_) as e -> Munop (Seqi zero,e)
  | Mbinop (Sub,e1,e2) -> Mbinop (Seq,e1,e2)
  | _ -> Mbinop (Seq,e1,e2)

and mk_sle e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m >= 0
        then Mconst one
        else Mconst zero
  | Mconst n,e ->
      if n=min_int
        then Mor (e, Mconst one)
        else Munop (Sgti (pred n), e)
  | e,Mconst n ->
      if n=max_int
        then Mor (e, Mconst one)
        else Munop (Slti (succ n), e)
  | e1,e2 -> Mbinop (Sle,e1,e2)

and mk_slt e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m > 0
        then Mconst one
        else Mconst zero
  | e, Mconst m -> Munop (Slti m,e)
  | Mconst m, e -> Munop (Sgti m,e)
  | e1,e2 -> Mbinop (Slt,e1,e2)

and mk_sne e1 e2 = match mk_sub e1 e2 with
  | Mconst n -> if n=zero then Mconst zero else Mconst one
  | Munop (Subi m, e) -> Munop (Snei m,e)
  | Munop (Addi m, e) -> Munop (Snei (neg m),e)
  | Munop (Neg, e) -> mk_sne e2 e1
  | Mbinop (Add,_,_) as e -> Munop (Snei zero,e)
  | Mbinop (Sub,e1,e2) -> Mbinop (Sne,e1,e2)
  | _ -> Mbinop (Sne,e1,e2)

let mk_load t_size ofs e =
  if t_size = 4
    then begin
      assert (logand ofs (of_int 3) = zero);
      Mlw (ofs,e)
    end
  else if t_size=1 then Mlb (ofs,e)
  else Mload (t_size,ofs,e)

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
  | Mloc i,e2 -> Munop (Move i,e2)
  | _ -> assert false

let mk_deref t_size = function
  | Munop (Addi n,e) -> mk_load t_size n e
  | Munop (Subi n,e) -> mk_load t_size (neg n) e
  | Maddr i -> Mloc i
  | e -> mk_load t_size zero e

let mk_la = function
  | Mlw (n,e)
  | Mlb (n,e)
  | Mload (_,n,e) -> mk_add (Mconst n) e
  | Mloc i -> Maddr i
  | _ -> assert false (* Not an l-value *)

let free_string = ref 0

let mk_string s =
  incr free_string;
  data := (!free_string,s)::!data;
  "string"^string_of_int !free_string


let mk_unop sz u x = match u with
  | Ast.Incrp -> mk_move x (Munop (Addi one,x))
  | Ast.Decrp -> mk_move x (Munop (Subi one,x))
  | Ast.Incrs -> mk_sub (mk_move x (mk_add x (Mconst one)))
                        (Mconst one)
  | Ast.Decrs -> mk_add (mk_move x (mk_sub x (Mconst one)))
                        (Mconst one)
  (* i++ ~ (i=i+1)-1*)
  | Ast.Not -> mk_not x
  | Ast.Star -> mk_deref sz x
  | Ast.Address -> mk_la x
  | Ast.Uminus -> mk_neg x
  | Ast.Uplus -> x

let mk_binop o e1 e2 = match o with
  | Ast.Eq  -> mk_seq e1 e2
  | Ast.Neq -> mk_sne e1 e2
  | Ast.Lt  -> mk_slt e1 e2
  | Ast.Leq -> mk_sle e1 e2
  | Ast.Gt  -> mk_slt e2 e1
  | Ast.Geq -> mk_sle e2 e1
  | Ast.Mul -> mk_mul e1 e2
  | Ast.Div -> mk_div e1 e2
  | Ast.Mod -> mk_rem e1 e2
  | Ast.And -> mk_and e1 e2
  | Ast.Or  -> mk_or  e1 e2
  | Ast.Add
  | Ast.Sub -> assert false

let rec isexpr {tdesc=e ; t=tt} = match e with
  | TCi n -> Mconst n
  | TLoc i -> Mloc i
  | TGlo x -> mk_load (sizeof tt) zero (Mla x)
  | TAssign (e1,e2) ->
    mk_move (isexpr e1) (isexpr e2)
  | TCall (f,l) -> Mcall (f,List.map isexpr l)
  | TUnop (u,e) -> mk_unop (sizeof tt) u (isexpr e)
  | TBinop (Ast.Add,e1,e2) ->
      begin match e1.t,e2.t with
      | P (n,t) as p,_ ->
          let sz = if n=1 then sizeof t else sizeof p in
          mk_add
          (isexpr e1)
          (mk_mul (Mconst (of_int sz)) (isexpr e2))
      | _,(P (n,t) as p) -> 
          let sz = if n=1 then sizeof t else sizeof p in
          mk_add
          (isexpr e2)
          (mk_mul (Mconst (of_int sz)) (isexpr e1))
      | C,C -> Munop (Andi byte, (mk_add (isexpr e1) (isexpr e2)))
      | _,_ -> mk_add (isexpr e1) (isexpr e2)
      end
  | TBinop (Ast.Sub,e1,e2) ->
      begin match e1.t with
      | P (n,t) as p ->
          let sz = if n=1 then sizeof t else sizeof p in
          mk_sub
          (isexpr e1)
          (mk_mul (Mconst (of_int sz)) (isexpr e2))
      | C -> Munop (Andi byte, (mk_sub (isexpr e1) (isexpr e2)))
      | _ -> mk_sub (isexpr e1) (isexpr e2)
      end
  | TBinop (o,e1,e2) -> mk_binop o (isexpr e1) (isexpr e2)
  | TSizeof t -> Mconst (of_int (sizeof t))
  | TCs s -> Mla (mk_string s)
  | TDot (e,i) -> match e.t with
    | S j -> let ofs = ofs j i in
        mk_deref (sizeof tt)
          (mk_add (mk_la (isexpr e))
          (Mconst ofs))
    | U j -> mk_deref (sizeof tt) (mk_la (isexpr e))
    | _ -> assert false

let rec isinstr t0 = function
  | TNop -> Nop
  | TExpr e -> Expr (isexpr e)
  | TIf (e,i1,i2) -> If (isexpr e,isinstr t0 i1,isinstr t0 i2)
  | TWhile (e,i) -> While (isexpr e,isinstr t0 i)
  | TFor (init,cond,inc,i) ->
      For (List.map isexpr init,isexpr cond,List.map isexpr inc,isinstr t0 i)
  | TBloc i -> Bloc (List.map (isinstr t0) i)
  | TReturn None -> Ret None
  | TReturn (Some e) ->
      Ret (Some (
      if t0=C && e.t<>C
        then Munop (Andi byte, (isexpr e))
        else isexpr e))


let isfct {
  tret=t;
  tfid=f;
  Typing.formals=argc;
  locals=lcl;
  tbody=il;
} = 
  {
    retsz=sizeof t;
    fid=f;
    formals=argc;
    locals=Array.length lcl;
    locsz=Array.map sizeof lcl;
    body=List.map (isinstr t) il;
  }

let gvars vl =
  List.map (fun (t,v) -> (sizeof t,v)) vl

(* We only need types lengths, typing guarantees
 * we know the data length *)
let isconstr = function
  | S i,s ->
      let sz = ref 0 in
      let f_loc = Array.make (Array.length s) zero in
      let align = ref false in
      for i = 0 to Array.length s - 1 do
        if aligned s.(i)
          then begin
            sz := ((!sz+3)/4)*4;
            align := true;
          end;
        sz := !sz + sizeof s.(i);
        f_loc.(i) <- of_int (!sz-4);
      done;
      Hashtbl.add constr i (!sz,!align,f_loc)
  | U i,u ->
      let s_max = ref 0 in
      let align = ref false in
      for i = 0 to Array.length u - 1 do
        s_max:=max !s_max (sizeof u.(i));
        if aligned u.(i)
          then align := true;
      done;
      Hashtbl.add constr i (!s_max,!align,[||])
  | _ -> assert false


let file (c,f,v) =
  Hashtbl.clear constr;
  data := [];
  List.iter isconstr c;
  let f,v = List.map isfct f,gvars v in
  f,v,!data
