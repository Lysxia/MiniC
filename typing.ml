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

module Sset = Set.Make(String)
 
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
  | TCi of Int32.t
  | TCs of string
  | TId of tident
  | TDot of texpr*tident
  | TAssign of texpr*texpr
  | TCall of tident*texpr list
  | TUnop of tunop*texpr
  | TBinop of tbinop*texpr*texpr
  | TSizeof of tt


type tvdec = tt*tident

type tinstr =
  | TNop
  | TExpr of texpr
  | TIf of texpr*tinstr*tinstr
  | TWhile of texpr*tinstr
  | TFor of texpr list*texpr*texpr list*tinstr
  | TBloc of tvdec list*tinstr list
  | TReturn of texpr option

type tconstr = tt*tt array

type tfct = tt*tident*tvdec list*tinstr (* a block *)

type tfile =
  tconstr list*tfct list*tvdec list
(*****)

(* Identifiers are converted to numbers,
 * this is for absolute efficiency only *)

type env = {
  f : (tt*tident*tt list) Smap.t ; (* functions *)
  constr : tt Smap.t; (* defined structure/union types *)
  mb : (tt*tident) Smap.t Imap.t ; (* s/u members *)
  v : (tt*tident) Smap.t ; (* variables *)
  free : int ;
  }

let empty = {
  f=Smap.empty;
  constr=Smap.empty;
  mb=Imap.empty;
  v=Smap.empty;
  free=0;}

let globals = ref empty

(* For error reporting only 
 * Find structure/union type identifier *)
let constr_of_tid = Hashtbl.create 7

let rec stringtype = function
  | V -> "void"
  | I -> "int"
  | C -> "char"
  | S s -> "struct "^(Hashtbl.find constr_of_tid s)
  | U s -> "union "^(Hashtbl.find constr_of_tid s)
  | P (n,t) -> (stringtype t)^String.make n '*'
  | Null -> "typenull"

let error loc s =
  raise (Error.E (loc.start_p,loc.end_p,s))

let constr_id = function
  | U i | S i -> i
  | _ -> raise (E "not a structure or union")
(*****)

(* Type relationships *)

let mkpnt = function
  | P (n,t) -> P (n+1,t)
  | t -> P (1,t)

let deref = function
  | P (1,t) -> t
  | P (n,t) when n>0 -> P (n-1,t)
  | _ -> raise (E "not a pointer")

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
  compatible t Null || compatible t (P (1,V))

(*****)

(* Typing *)

(* gcc does not allow struct and union
 * with the same identifier, so we don't either *)
let rec wft (* well formed type*) env = function
  | Void -> V
  | Int -> I
  | Char -> C
  | Point (n,t) -> P (n,wft env t)
  | Struct s ->
      begin
        try
          match Smap.find s env.constr with
            | S i -> S i
            | _ -> raise Not_found
        with
          Not_found -> raise (E (s^
            " is not a structure identifier"))
      end
  | Union s ->
      try
        match Smap.find s env.constr with
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
let typedot env e i =
  try
    let t,i = Smap.find i (Imap.find (constr_id e.t) env.mb) in
    mkt (TDot (e,i)) t
  with
    | E s -> raise (E ("request for member \'"^i^"in something "^s))
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
  let rec matchtype i tl el = match tl,el with
    | [],[] -> ()
    | t::tt,e::et when compatible t e.t -> matchtype (i+1) tt et
    | [],_ -> raise (E "This function is applied to too many arguments")
    | _,[] -> raise (E "This function is applied to too few arguments")
    | t::_,e::_ -> raise (E
        ("Argument "^(string_of_int i)^" has type \'"^
          (stringtype e.t)^
          "\' but an expression was expected of type \'"^
          (stringtype t)^"\'"))
  in
  fun env f el ->
  try
    let rett,id,argt = Smap.find f env.f in
    matchtype 1 argt el;
    mkt (TCall (id,el)) rett
  with
    Not_found -> raise (E ("\'"^f^"\' is not a function"))

let typeunop o e = match o with
  | Incrp | Incrs | Decrp | Decrs ->
      let lv = lvalue e.tdesc and n = num e.t in
      if lv && n
        then mkt (TUnop (o,e)) e.t
      else if not lv
        then raise (E "lvalue required as incr/decrement operand")
      else raise (E "wrong argument type as incr/decrement operand")
  | Address -> if lvalue e.tdesc then mkt (TUnop (Address,e)) (mkpnt e.t)
      else raise (E "lvalue required as unary '&' operand")
  | Not -> if num e.t then mkt (TUnop (Not,e)) I
      else raise (E "wrong type argument to unary '!'")
  | Uminus | Uplus -> if compatible e.t I then mkt (TUnop (o,e)) I
      else raise (E ("wrong type argument to unary \'"^
        (if o=Uminus then "-" else "+")^"\'"))
  | Star -> mkt (TUnop (o,e)) (deref e.t)

(* Addition and substraction rules create a lot of cases to be checked *)
let typebinop =
  let arith_rule o (* (+,-,*,/,%,||,&&) *) e1 e2 =
    if compatible e1.t e2.t && compatible e1.t I
      then mkt (TBinop (o,e1,e2)) I
      else raise (E
        ("invalid operands to binary operator (have \'"^
        (stringtype e1.t)^"\' and \'"^(stringtype e2.t)^"\')"))
  in
  let add_rule (* (+) *) e1 e2 =
    match e1.t,e2.t with
      | P (n,t),u | u,P (n,t) when compatible u I
        -> mkt (TBinop (Add,e1,e2)) (P (n,t))
      | _,_ -> try arith_rule Add e1 e2 
        with E _ -> arith_rule Add e2 e1
  in
  let sub_rule (* (-) *) e1 e2 =
    match e1.t,e2.t with
      | P (n,t),P (m,u) when m=n && t=u ->
        mkt (TBinop (Sub,e1,e2)) I
      | P (n,t),u when compatible u I ->
        mkt (TBinop (Sub,e1,e2)) (P (n,t))
      | _,_ -> arith_rule Sub e1 e2
  in
  fun o e1 e2 -> match o with
  | Eq | Neq | Lt | Leq | Gt | Geq ->
      if compatible e1.t e2.t && num e1.t
        then mkt (TBinop (o,e1,e2)) I
        else raise (E
          ("invalid operands to binary (bool) operator (have \'"^
          (stringtype e1.t)^"\' and \'"^(stringtype e2.t)^"\')"))
  | Mul | Div | Mod | And | Or ->
      arith_rule o e1 e2
  | Add -> add_rule e1 e2
  | Sub -> sub_rule e1 e2

(* Main expression typing function *)
let rec typeexpr env { desc=edesc ; loc=loc } =
  try
    match edesc with
      | Cint i when i=Int32.zero -> mkt (TCi Int32.zero) Null
      | Cint i -> mkt (TCi i) I
      | Cstring s -> mkt (TCs s) (P (1,C))
      | Ident x ->
          let t,i =
            try 
              Smap.find x env.v
            with
              | Not_found -> error loc ("\'"^x^"\' undeclared")
          in
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
          let t = wft env t in
          if compatible t V
            then raise (E "type \'void\' has no size")
            else mkt (TSizeof t) I
  with
    | E s -> error loc s

(* Function for debugging purposes only *)
let type_expr = typeexpr empty
(*****)

(* Variable declaration *)
let typevdec env { desc=vt,vid ; loc=loc } =
  try
    let vt = wft env vt in
    if compatible vt V
      then
        raise (E ("variable or field \'"^vid^"\' declared void"))
      else { env with
        v=Smap.add vid (vt,env.free) env.v;
        free=env.free+1 }
  with
    | E s -> error loc s

(* Checks name unicity
 * (in a declaration sequence inside one block
 * or among function arguments) *)
let typevdeclist env vl =
  let rec uni env tvl s = function
  | [] -> env,List.rev tvl
  | ({ desc=_,vid ; loc=loc } as h)::t ->
      if Sset.mem vid s
        then error loc
          ("previous declaration of \'"^vid^"\' was here")
        else ( let env = typevdec env h in
          uni env ((Smap.find vid env.v)::tvl)
          (Sset.add vid s) t)
  in
  uni env [] Sset.empty vl

let typeglobalvdec env ({ desc=vt,id ; loc=loc } as h) =
  if Smap.mem id env.v
    then error loc
      ("redefinition of \'"^id^"\'")
  else if Smap.mem id env.f
    then error loc
      ("\'"^id^"\' redeclared as different kind of symbol")
    else typevdec env h
  
(*****)

(* Instruction typing *)
let rec typeinstr t0 env = function
  | Expr e -> TExpr (typeexpr env e)
  | Instr i -> typei t0 env i

and typei t0 env { desc=idesc ; loc=loc } =
  try
    match idesc with
      | Nop -> TNop
      | If (e,i1,i2) -> let e = typeexpr env e in
          if num e.t
            then TIf (e,typeinstr t0 env i1,typeinstr t0 env i2)
            else raise (E ("used \'"^(stringtype e.t)^
              "\' type value where scalar is required"))
      | While (e,i) -> let e = typeexpr env e in
          if num e.t
            then TWhile (e,typeinstr t0 env i)
            else raise (E ("used \'"^(stringtype e.t)^
              "\' type value where scalar is required"))
      | For (el1,e,el2,i) -> let e = match e with
            | None -> mkt (TCi Int32.one) I
            | Some e -> typeexpr env e in
          if num e.t
            then TFor (
              List.map (typeexpr env) el1, e,
              List.map (typeexpr env) el2, typeinstr t0 env i)
            else raise (E ("used \'"^(stringtype e.t)^
              "\' type value where scalar is required"))
      | Bloc (vl,il) -> let env,tvl = typevdeclist env vl in
          TBloc (tvl,List.map (typeinstr t0 env) il)
      | Return None ->
          (*if t0=V then gcc accepts*) TReturn None
      | Return (Some e) -> let e = typeexpr env e in
          if t0=V
            then raise (E
              ("\'return\' with a value, "^
              "in function returning void"))
          else if compatible e.t t0
            then TReturn (Some e)
            else raise (E
              ("incompatible types when returning type \'"^
               (stringtype e.t)^"\' but \'"^(stringtype t0)^
               "\' was expected"))
  with E s -> error loc s

(* Function for debugging purposes only *)
let type_instr = typeinstr V empty
(*****)

(* Functions and Constructors *)
let typeconstr =
  let free = ref 0 in
  fun env (t,vl) ->
  let n = List.length vl in
  let members = Array.make n V in
  let t,id = match t with
    | Struct id -> (S !free),id
    | Union id -> (U !free),id
    | _ -> assert false
  in
  if Smap.mem id env.constr
    then raise (E 
      ("structure or union with the same name \'"^id^
       "\' previously declared"));
  let env = { env with constr=Smap.add id t env.constr } in
  let rec fill i j s mb = function
    | [] -> mb
    | { desc=vt,id ; loc=loc }::tl -> try
      let vt=wft env vt in
      if compatible vt V
        then raise (E ("variable or field \'"^id^"\' declared void"))
      else if Sset.mem id s
        then raise (E ("duplicate member \'"^id^"\'"))
      else if vt = t
        then raise (E ("Incomplete type \'"^id^"\'"))
      else if vt = C
        then begin
          members.(j) <- vt;
          fill i (j-1) (Sset.add id s) (Smap.add id (vt,j) mb) tl
        end
        else begin
          members.(i) <- vt;
          fill (i+1) j (Sset.add id s) (Smap.add id (vt,i) mb) tl
        end
    with
       | E s -> error loc s
  in
  Hashtbl.add constr_of_tid !free id;
  let env = { env with mb=Imap.add !free (fill 0 (n-1) Sset.empty Smap.empty vl) env.mb } in
  incr free;
  env, (t,members)

let typefun =
  let free = ref 0 in
  fun env loc (t,id,arg,decl,instr) ->
    if Smap.mem id env.f
      then raise (E
        ("redefinition of \'"^id^"\'"))
    else if Smap.mem id env.v
      then raise (E
        ("\'"^id^"\' redeclared as different kind of symbol"));
    let t = wft env t in
    let env2,arg = typevdeclist env arg in
    let argt = List.map (fun (t,_) -> t) arg in
    let new_ef = Smap.add id (t,!free,argt) env.f in
    let env2 = { env2 with f=new_ef } in
    let tf = t,!free,arg,typei t env2 {desc=Bloc(decl,instr);loc=loc} in
    incr free;
    { env with f=new_ef },tf
(*****)

(* Program typing *)
let type_prog { desc=ast ; loc=loc } =
  let rec main_loc = function
    | Dec { desc=Fct (_,"main",_,_,_) ; loc=loc }::_ ->
      loc.start_p,loc.end_p
    | _::t -> main_loc t
    | [] -> raise Not_found
  in
  let rec type_declist env cl fl = function
    | [] -> (try
        match Smap.find "main" env.f with
          | I,_,[] | I,_,[I;P(2,C)] -> 
            let vl =
              Smap.fold (fun _ d vl -> d::vl) env.v []
            in cl,fl,vl
          | _ -> let sp,ep = main_loc ast in
          raise (Error.E (sp,ep,
          "function main has incorrect prototype"));
      with
        | Not_found -> raise (Error.E (loc.end_p,loc.end_p,
        "int main() or int main(int,char**) expected")))
    | (Ast.V vd)::t -> type_declist (typeglobalvdec env vd) cl fl t
    | (Dec {desc=d;loc=loc})::t -> try
      match d with
        | Typ (ty,v) -> let env,c = typeconstr env (ty,v) in
            type_declist env (c::cl) fl t
        | Fct (rt,id,arg,d,i) ->
          let env,f = typefun env loc (rt,id,arg,d,i) in
            type_declist env cl (f::fl) t
      with E s -> error loc s
  in
  type_declist { empty with
    f=Smap.add "putchar" (I,-1,[I])
      (Smap.singleton "sbrk" (P(1,V),-2,[I])) }
    [] [] ast
  
(*****)

(*____\o/_______________/[_____S_H_A_R_K___A_T_T_A_C_K___!___*)
