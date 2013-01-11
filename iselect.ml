(** Mini-C Compiler **)
(* Li-yao Xia *)

open Typing
open Int32
(* ^ This module is intensively used since we do a lot of evaluation *)

module Iset = Set.Make(struct type t=int let compare=Pervasives.compare end)

type munop =
  | Neg
  | Divi of t
  | Remi of t
  | Addi of t
  | Subi of t
  | Muli of t

  | Slti of t
  | Seqi of t
  | Snei of t
  | Sgti of t
  | Andi of t

  | Sltiu of t

  | Srl of int
  | Sll of int

type mbinop =
  | Add | Div | Mul | Sub | Rem | Seq | Sne | Slt | Sle | Sltu | Or
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
  | Mla     of string
  | Maddr   of tident (*snd:size*)
  | Mload   of bool*int*t*expr (* for some size *)
  | Mstor   of bool*int*expr*t*expr
  | Mand    of expr*expr
  | Mor     of expr*expr
  | Mcall   of int*string*expr list
  | Mcall_addr of int*string*expr list

type instr =
  | Nop
  | Expr  of expr
  | If    of expr*instr*instr
  | While of expr*instr
  | For   of expr list*expr*expr list*instr
  | Bloc  of instr list
  | Ret   of expr option

type vdec = int*string

type data =
  | Str of Ast.str
  | Space of int

type fct = {
  retsz:int;
  retal:bool;
  fid:string;
  formals:int;
  locals:int;
  locsz:(bool*int) array;
  body:instr
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

let data = ref []

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
  | Mconst _ | Mla _ | Maddr _ -> true
  | Mload _ | Mstor _ -> false
  | Munop (_,e) -> pure e
  | Mcall _ | Mcall_addr _ -> false
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
      else if n=one
        then e
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
        then Printf.printf "Warning : Divide by zero\n";
      Munop (Divi n,e)
  | e1,e2 -> Mbinop (Div, e1, e2)

and mk_rem e1 e2 = match e1,e2 with
  | Mconst n, Mconst m when m<>zero ->
      Mconst (rem n m)
  | e1,Mconst n ->
      if n=zero
        then Printf.printf "Warning : Divide by zero\n";
      Munop (Remi n,e1)
  | e1,e2 -> Mbinop (Rem, e1, e2)

(* bool op *)
let rec mk_not e = match e with
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

let mk_load t ofs e =
  Mload (aligned t,sizeof t,ofs,e)

let mk_move e1 e2 = match e1,e2 with
  | Mload (a,s,ofs,e1),e2 -> Mstor (a,s,e2,ofs,e1)
  | _ -> assert false

(* We will force spilling *)
let mk_deref t e = match e with
  | Munop (Addi n,e) -> mk_load t n e
  | Munop (Subi n,e) -> mk_load t (neg n) e
  | e -> mk_load t zero e

let mk_la e = match e with
  | Mload (_,_,n,e) -> mk_add (Mconst n) e
  | Mcall (s,f,el) -> Mcall_addr (s,f,el)
  | _ -> assert false

let free_string = ref 0

let mk_string s =
  incr free_string;
  let id = "string"^string_of_int !free_string in
  data := (id,Str s)::!data;
  id

let mk_t_add t1 t2 e1 e2 = match t1,t2 with
  | P (n,t) as p,_ ->
      let sz = if n=1 then sizeof t else sizeof p in
      mk_add e1 (mk_mul (Mconst (of_int sz)) e2)
  | _,(P (n,t) as p) -> 
      let sz = if n=1 then sizeof t else sizeof p in
      mk_add e2 (mk_mul (Mconst (of_int sz)) e1)
  | _,_ -> mk_add e1 e2

let mk_t_sub t1 t2 e1 e2 = match t1,t2 with
  | P _,P _ -> mk_sub e1 e2
  | (P (n,t) as p),_ ->
      let sz = if n=1 then sizeof t else sizeof p in
      mk_sub e1 (mk_mul (Mconst (of_int sz)) e2)
  | _ -> mk_sub e1 e2

let one = Mconst one

let mk_unop t u x =
  match u with
  | Ast.Incrp -> mk_move x (mk_t_add t I x one)
  | Ast.Decrp -> mk_move x (mk_t_sub t I x one)
  | Ast.Incrs -> mk_t_sub t I (mk_move x (mk_t_add t I x one)) one
  | Ast.Decrs -> mk_t_add t I (mk_move x (mk_t_sub t I x one)) one
  (* i++ ~ (i=i+1)-1*)
  | Ast.Not -> mk_not x
  | Ast.Star ->
    begin
      match t with
        | P (n,t) -> mk_deref (if n=1 then t else P (n-1,t)) x
        | _ -> assert false
    end
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

let rec isexpr {tdesc=e;t=t} = match e with
  | TCi n -> Mconst n
  | TLoc i -> mk_deref t (Maddr i)
  | TGlo x -> mk_load t zero (Mla ("_"^x))
  | TAssign (e1,e2) -> mk_move (isexpr e1) (isexpr e2)
  | TCall (f,l) -> Mcall (sizeof t,f,List.map isexpr l)
  | TUnop (u,e) -> mk_unop e.t u (isexpr e)
  | TBinop (Ast.Add,e1,e2) -> mk_t_add e1.t e2.t (isexpr e1) (isexpr e2)
  | TBinop (Ast.Sub,e1,e2) -> mk_t_sub e1.t e2.t (isexpr e1) (isexpr e2)
  | TBinop (o,e1,e2) -> mk_binop o (isexpr e1) (isexpr e2)
  | TSizeof t -> Mconst (of_int (sizeof t))
  | TCs s -> Mla (mk_string s)
  | TDot (e,i) -> match e.t with
    | S j -> let ofs = ofs j i in
        mk_deref t
          (mk_add (mk_la (isexpr e))
          (Mconst ofs))
    | U j -> mk_deref t (mk_la (isexpr e))
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
      (*if t0=C && e.t<>C
        then Munop (Andi byte, (isexpr e))
        else*) isexpr e))


let isfct {
  tret=t;
  tfid=f;
  Typing.formals=argc;
  locals=lcl;
  tbody=i;
} = 
  {
    retsz=sizeof t;
    retal=aligned t;
    fid=f;
    formals=argc;
    locals=Array.length lcl;
    locsz=Array.map (fun x->aligned x,sizeof x) lcl;
    body=isinstr t i;
  }

let gvars vl =
  List.iter (fun (t,v) -> data:=("_"^v,Space (sizeof t))::!data) vl

(* Next multiple of 4 *)
let round_4 i = (i+3)/4*4

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
            sz := round_4 !sz;
            align := true;
          end;
        f_loc.(i) <- of_int !sz;
        sz := !sz + sizeof s.(i);
      done;
      if !align
        then sz := round_4 !sz;
      Hashtbl.add constr i (!sz,!align,f_loc)
  | U i,u ->
      let s_max = ref 0 in
      let align = ref false in
      for i = 0 to Array.length u - 1 do
        s_max:=max !s_max (sizeof u.(i));
        if aligned u.(i)
          then align := true;
      done;
      if !align
        then s_max := (!s_max+3)/4*4;
      Hashtbl.add constr i (!s_max,!align,[||])
  | _ -> assert false

let isprog (c,f,v) =
  List.iter isconstr c;
  gvars v;
  List.map isfct f


(**)
let reset () =
  Typing.reset ();
  Hashtbl.clear constr;
  data := []

