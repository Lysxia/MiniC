(** Mini-C Compiler **)
(* Li-yao Xia *)

(* Variables are stored in pseudo-registers *)
(* instruction 'la' will force a variable to be on the stack *)

open Typing

module Iset = Set.Make(struct type t=int let compare=compare end)

type munop =
  | Neg
  | Divi of Int32.t (* WRITTEN div IN MIPS *)
  | Remi of Int32.t
  | Addi of Int32.t | Muli of Int32.t | Subi of Int32.t
  | Slti of Int32.t | Sltiu of Int32.t | Sll of int
  (* sltiu $1,$2,1 is in C : $1=!$2*)

type mbinop =
  | Add | Div | Mul | Sub | Rem | Slt | Sle | Sltu
  (* slt, sltu, sltiu, slti are basic instructions,
   * others are pseudo-instruction
   * (and seq is not optimally translated)
   * we simplified the set of bool operators *)
  (* sltu $1, $zero, $2 is $1=($2!=0) also $1=!!$2 *)

(* unsigned addu and subu do not overflow...
 * those are what we will really use *)

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

(**********************************************)

(* returns true and k if x=2^k *)
let log2 x =
  let rec log2 x k =
    if x=Int32.one then true,k
    else if Int32.logand Int32.one x = Int32.one then false,k
    else log2 (Int32.shift_right_logical x 1) (k+1)
  in
  if compare x Int32.zero > 0 then log2 x 0 else false,-1

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
  | _ -> assert false

let ofs i j =
  let _,_,fields = Hashtbl.find constr i in
  fields.(j)

(* Return a couple (b:bool,e:expr) where
 * - b=true if e is necessarily pure
 * - else e may not be pure *)
(* We could optimize out useless operations here,
 * but it will be done more efficiently in the next step *)
let rec pure e = match e with
  | Munop (_,e) -> pure e
  | Mbinop (_,e1,e2)
  | Mand (e1,e2)
  | Mor (e1,e2) -> pure e1 && pure e2
  | Mconst _ | Mloc _ | Mla _ | Maddr _ -> true
  | Mmove _
  | Mlw _ | Mlb _ | Mload _ | Msw _ | Msb _ | Mstor _ -> false
  | Mcall _ -> false
  (* Functions could be examined for pureness *)

(* We use the fact that the evaluation order of operands
 * of a binary operator is unspecified *)
let rec mk_add e1 e2 = match e1,e2 with
  | Mconst m, Mconst n          -> Mconst (Int32.add m n)
  | e, Mconst n
  | Mconst n, e when n = Int32.zero -> e
  | Munop (Addi m, e), Mconst n
  | Mconst n, Munop (Addi m, e) -> mk_add (Mconst (Int32.add m n)) e
  | Munop (Addi m, e1), e2
  | e2, Munop (Addi m, e1)      -> mk_add (Mconst m) (mk_add e1 e2)
  | Munop (Subi m, e), Mconst n
  | Mconst n, Munop (Subi m, e) -> mk_add (Mconst (Int32.sub n m)) e
  | Munop (Subi m, e1), e2
  | e2, Munop (Subi m, e1)      -> mk_sub (mk_add e1 e2) (Mconst m)
  | e1, Munop (Neg, e2)
  | Munop (Neg, e2), e1         -> mk_sub e1 e2
  | Munop (Muli m, e1), Munop (Muli n, e2) when m=n ->
      mk_mul (Mconst m) (mk_add e1 e2)
  | Munop (Sll k, e1), Munop (Sll l, e2) when k=l ->
      mk_mul (Mconst (Int32.shift_left Int32.one k)) (mk_add e1 e2)
  (* This doesnt work for div because strictly speaking
   * div isn't a division *)
  | Mbinop (Sub, Mconst m, e), Mconst n
  | Mconst n, Mbinop (Sub, Mconst m, e) ->
      Mbinop (Sub, Mconst (Int32.add m n), e)
  | Mbinop (Sub, Mconst m, e1), e2
  | e2, Mbinop (Sub, Mconst m, e1) ->
      mk_add (Mconst m) (mk_sub e2 e1)
  | e,Mconst n | Mconst n,e -> Munop (Addi n, e)
  | _ -> Mbinop (Add, e1, e2)

and mk_sub e1 e2 = match e1,e2 with
  | Mconst m, Mconst n -> Mconst (Int32.sub m n)
  | e, Mconst n when n=Int32.zero -> e
  | Mconst n, e when n=Int32.zero -> mk_neg e
  | Munop (Subi m, e), Mconst n -> mk_sub e (Mconst (Int32.add m n))
  | Mconst n, Munop (Subi m, e) -> mk_sub (Mconst (Int32.add m n)) e
  | Munop (Addi n, e), Mconst m -> mk_sub e (Mconst (Int32.sub m n))
  | Mconst m, Munop (Addi n, e) -> mk_sub (Mconst (Int32.sub m n)) e
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
  | Mconst n -> Mconst (Int32.neg n)
  | Munop (Neg, e) -> e
  | Mbinop (Sub, e1, e2)      -> mk_sub e2 e1
  | Munop (Muli n, e)         -> Munop (Muli (Int32.neg n), e)
  | Mbinop (Div, Mconst n, e) -> Mbinop (Div, Mconst (Int32.neg n), e)
  | e -> Munop (Neg, e)

and mk_mul e1 e2 = match e1,e2 with
  | Mconst n, Mconst m -> Mconst (Int32.mul n m)
  | Mconst n, Munop (Muli m, e)
  | Munop (Muli m, e), Mconst n -> mk_mul (Mconst (Int32.mul m n)) e
  | Mconst n, Munop (Sll k, e)
  | Munop (Sll k, e), Mconst n ->
      mk_mul (Mconst (Int32.mul (Int32.shift_left Int32.one k) n)) e
  | Munop (Addi m, e1), (Mconst n as e2)
  | (Mconst n as e2), Munop (Addi m, e1) ->
      mk_add (mk_mul e1 e2) (Mconst (Int32.mul m n))
  | Munop (Subi m, e1), (Mconst n as e2)
  | (Mconst n as e2), Munop (Subi m, e1) ->
      mk_sub (mk_mul e1 e2) (Mconst (Int32.mul m n))
  | e2,Munop (Neg,e1) -> mk_neg (mk_mul e1 e2)
  | Mconst n, e | e, Mconst n ->
      if n=Int32.zero && pure e
        then Mconst Int32.zero
        else let p,k = log2 n in
          if p then Munop (Sll k, e) else Munop (Muli n, e)
  | Munop (Neg,e1),e2
  | e1, e2 -> Mbinop (Mul, e1, e2)
 (* use shifts when mul a power of 2 *)
 (* e*0 -> check pure e*)

and mk_div e1 e2 = match e1,e2 with
  | Mconst n, Mconst m when m<>Int32.zero ->
      Mconst (Int32.div n m)
  | e, Mconst n ->
      if n=Int32.zero
        then Printf.printf "Warning : Divide by zero";
      Munop (Divi n,e)
  | e1,e2 -> Mbinop (Div, e1, e2)

and mk_rem e1 e2 = match e1,e2 with
  | Mconst n, Mconst m when m<>Int32.zero ->
      Mconst (Int32.rem n m)
  | e1,Mconst n ->
      if n=Int32.zero
        then Printf.printf "Warning : Divide by zero";
      Munop (Remi n,e1)
  | e1,e2 -> Mbinop (Rem, e1, e2)

(* bool op *)
(* sltiu with 1 is "equal to zero" *)
let rec mk_not = function
  | Mconst n ->
      if n=Int32.zero
        then Mconst Int32.one
        else Mconst Int32.zero
  | Munop (Neg,e) -> mk_not e
  | Munop (Sltiu n,e) -> begin
      match e with
      (* That turned out to be exact in any case *)
      | Mbinop (Slt,_,_) | Mbinop (Sle,_,_) | Mbinop (Sltu,_,_)
      | Mor _ | Mand _ when n=Int32.one -> e
      | _ -> Mbinop (Sltu, Mconst (Int32.pred n), e)
  end
  | Munop (Slti n,e) ->  mk_sle (Mconst n) e
  | Mbinop (Sltu, e, Mconst n) -> Munop (Sltiu (Int32.succ n),e)
  (* Other cases should not happen (they fit into the last pattern)
   * because unsigned comparison is weird and unused elsewhere *)
  | Mbinop (Slt,e1,e2) -> mk_sle e2 e1
  | Mbinop (Sle,e1,e2) -> mk_slt e2 e1
  | e -> Munop (Sltiu Int32.one, e)

and mk_seq e1 e2 = mk_not (mk_sub e1 e2)

and mk_sle e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m >= 0
        then Mconst Int32.one
        else Mconst Int32.zero
  | (Mconst n as e1),e2 ->
      if n=Int32.min_int
        then Mbinop (Sle,e1,e2)
        else Mbinop (Slt,Mconst (Int32.pred n),e2)
  | e1,(Mconst n as e2) ->
      if n=Int32.max_int
        then Mbinop (Sle,e1,e2)
        else Mbinop (Slt,e1,Mconst (Int32.succ n))
  | e1,e2 -> Mbinop (Sle,e1,e2)

and mk_slt e1 e2 = match e1,e2 with
  | Mconst n,Mconst m ->
      if compare n m > 0
        then Mconst Int32.one
        else Mconst Int32.zero
  | e, Mconst m ->
      Munop (Slti m,e)
  | e1,e2 -> Mbinop (Slt,e1,e2)

and mk_sne e1 e2 = mk_not (mk_seq e1 e2)

let mk_bool e = mk_not (mk_not e)

(* Guarantee left to right evaluation *)
let mk_and e1 e2 = match e1,e2 with
  | Mconst n,e2 ->
      if n=Int32.zero
        then Mconst Int32.zero
        else mk_bool e2
  | e1,e2 -> Mand (e1,e2)

let mk_or e1 e2 = match e1,e2 with
  | Mconst n,e2 ->
      if n=Int32.zero
        then mk_bool e2
        else Mconst Int32.one
   | e1,e2 -> Mor (e1,e2)

let mk_load t_size ofs e =
  if t_size = 4
    then begin
      assert (Int32.logand ofs (Int32.of_int 3) = Int32.zero);
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
  | e1,e2 -> Mmove (e1,e2)

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
  | _ -> assert false (* Not an l-value *)

let free_string = ref 0

let mk_string s =
  incr free_string;
  data := (!free_string,s)::!data;
  "string"^string_of_int !free_string


let mk_unop sz u x = match u with
  | Ast.Incrp -> Mmove (x,Munop (Addi Int32.one,x))
  | Ast.Decrp -> Mmove (x,Munop (Subi Int32.one,x))
  | Ast.Incrs -> mk_sub (mk_move x (mk_add x (Mconst Int32.one)))
                        (Mconst Int32.one)
  | Ast.Decrs -> mk_add (mk_move x (mk_sub x (Mconst Int32.one)))
                        (Mconst Int32.one)
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
  | TGlo x -> mk_load (sizeof tt) Int32.zero (Mla x)
  | TAssign (e1,e2) ->
    mk_move (isexpr e1) (isexpr e2)
  | TCall (f,l) -> Mcall (f,List.map isexpr l)
  | TUnop (u,e) -> mk_unop (sizeof tt) u (isexpr e)
  | TBinop (Ast.Add,e1,e2) ->
      begin match e1.t,e2.t with
      | P _,_ -> mk_add
          (isexpr e1)
          (mk_mul (Mconst (Int32.of_int (sizeof e1.t))) (isexpr e2))
      | _,P _ -> mk_add 
          (isexpr e2)
          (mk_mul (Mconst (Int32.of_int (sizeof e2.t))) (isexpr e1))
      | _,_ -> mk_add (isexpr e1) (isexpr e2)
      end
  | TBinop (Ast.Sub,e1,e2) ->
      begin match e1.t with
      | P _ -> mk_sub
          (isexpr e1)
          (mk_mul (Mconst (Int32.of_int (sizeof e1.t))) (isexpr e2))
      | _ -> mk_sub (isexpr e1) (isexpr e2)
      end
  | TBinop (o,e1,e2) -> mk_binop o (isexpr e1) (isexpr e2)
  | TSizeof t -> Mconst (Int32.of_int (sizeof t))
  | TCs s -> Mla (mk_string s)
  | TDot (e,i) -> match e.t with
    | S j -> let ofs = ofs j i in
        mk_deref (sizeof tt)
          (mk_add (mk_la (isexpr e))
          (Mconst ofs))
    | U j -> mk_deref (sizeof tt) (mk_la (isexpr e))
    | _ -> assert false

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

(* We only need types lengths, typing guarantees
 * we know the data length *)
let isconstr = function
  | S i,s ->
      let sz = ref 0 in
      let f_loc = Array.make (Array.length s) Int32.zero in
      let align = ref false in
      for i = 0 to Array.length s - 1 do
        if aligned s.(i)
          then begin
            sz := ((!sz+3)/4)*4;
            align := true;
          end;
        sz := !sz + sizeof s.(i);
        f_loc.(i) <- Int32.of_int (!sz-4);
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
