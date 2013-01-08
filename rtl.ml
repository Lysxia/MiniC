open Iselect

module R = struct
  type t = int
  type mat =
    | R of t (* register nb (< 32 if physical, otherwise pseudo) *)
    | F of int
    | S of int
  let free = ref 31
  let fresh () = incr free; R !free
  let z  = R 0 (* is a special one *)
  let v0 = R 2
  let a0 = R 4
  let gp = R 28
  let sp = R 29
  let ra = R 31
  let callee_saved = [R 16;R 17;R 18;R 19;R 20;R 21;R 22;R 23]
  let caller_saved = List.map (fun x -> R x)
    [2;3;4;5;6;7;8;9;10;11;12;13;14;15;24;25;30;31]
  let compare a b=match a,b with
    | R a,R b | F a,F b | S a,S b -> compare a b
    | R _,F _ | R _,S _ | F _,S _ -> 1
    | F _,R _ | S _,R _ | S _,F _ -> -1
  let reset () =
    free := 31;
  module S = Set.Make(struct
    type t=mat let compare=compare end)
  module M = Map.Make(struct
    type t=mat let compare=compare end)
end

module L = struct
  type t = int
  let free = ref 0
  let fresh () = incr free; !free
  module M = Map.Make(struct type t=int let compare=compare end)
end

(*****************)

type reg = R.mat
type lab = L.t
type i32 = Int32.t

type u=munop
type b=mbinop
type ubranch =
  | Bgtz | Bgtzal | Bgez | Bgezal | Blez | Bltz | Bltzal | Beqz | Bnez
  | Beqi of i32 | Beqzi | Blti of i32 | Bgti of i32 | Bnei of i32
type bbranch = Beq | Bne | Blt | Ble

 (* We will be using $fp for moving things *)
type instr =
  | Const of reg*i32        *lab
  | Move  of reg*reg        *lab
  | Unop  of reg*u*reg      *lab
  | Binop of reg*b*reg*reg  *lab
  | La    of reg*string     *lab
  | Addr  of reg*reg        *lab
  | Load  of reg*bool*int*i32*reg*lab
  | Stor  of reg*bool*reg*int*i32*reg*lab
  | Jump  of lab
  | Ubch  of ubranch*reg*lab*lab
  | Bbch  of bbranch*reg*reg*lab*lab
  | Call  of reg*string*reg list*lab
  | Syscall of lab
  | Alloc_frame of lab
  | Free_frame of lab
  | ECall of string*lab
  | ETCall of string
  | Return
  | Label of string*lab
  | Blok of instr list

type graph = instr L.M.t

(* As is stated below, we do final call, and for efficiency we
 * will use two start labels in functions which need callee-saved
 * registers, so that we try not to store them twice,
 * we also keep track of actually USED caller_saved registers
 * so that we do not have to store them all as caller *)
type fct = {
  mutable ident:string;
  mutable s_ident:string;
  formals:reg list;
  locals:reg array;
  locsz:int array;
  args:reg list; (* Registers from a caller's perspective *)
  mutable return:reg; (* location of return *)
  mutable entry:lab;
  mutable exit:lab;
  mutable body:graph;
  mutable cr_saved:reg list; (* must only be R construct *)
  mutable ce_saved:(reg*reg) list;
  (* fst : physical register (< 32)
   * snd : storage location (either physical in frame or
   * pseudo reg in the case of recursive functions not yet in LTL)*)
}

(***********************************************)
(* WE IMPLEMENT FINAL CALL FOR ANY FUNCTION *)


(* As was done in class *)
let graph = ref L.M.empty
let locals = Hashtbl.create 14
let to_spill = ref R.S.empty

let init () =
  to_spill := R.S.empty;
  graph := L.M.empty;
  Hashtbl.clear locals;
  R.reset ();
  L.free := 0

let find_loc = Hashtbl.find locals
let spill ri = to_spill := R.S.add ri !to_spill

let generate i =
  let l = L.fresh() in
  graph := L.M.add l i !graph;
  l

let zero = Int32.zero
let one = Int32.one

let rec expr r e l =
  match e with
  | Mconst m -> generate (Const (r,m,l))
  | Munop (Iselect.Move i,e) ->
      let ri = find_loc i in
      expr ri e (generate (Move (r,ri,l)))
  | Munop (u,e) ->
      let r_ = R.fresh () in
      expr r_ e (generate (Unop (r,u,r_,l)))
  | Mbinop (o,e1,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r1 e1 (expr r2 e2 (generate (Binop (r,o,r1,r2,l))))
  | Mloc i -> generate (Move (r,find_loc i,l))
  | Mla x -> generate (La (r,x,l))
  | Maddr i ->
      let ri = find_loc i in
      spill ri;
      generate (Addr (r,ri,l))
  | Mload (a,sz,ofs,e) ->
      let r_ = R.fresh () in
      expr r_ e (generate (Load (r,a,sz,ofs,r_,l)))
  | Mstor (a,sz,e1,ofs,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r2 e2 (expr r1 e1 (generate (Stor (r,a,r1,sz,ofs,r2,l))))
  | Mand _
  | Mor _ ->
      condition e (generate (Const (r,one,l)))
                  (generate (Const (r,zero,l)))
  | Mcall (f,argl) ->
      let l0 = L.fresh () in
      let (rl,entry) = List.fold_right (
        fun e (rl,l) ->
          let r=R.fresh() in
            (r::rl,expr r e l)) argl ([],l0) in
      graph := L.M.add l0 (Call (r,f,rl,l)) !graph;
      entry

and condition e lif lelse =
  match e with
  | Mconst m -> if m=Int32.zero then lelse else lif
  | Munop (Muli zero,e) ->
      expr (R.fresh ()) e lelse
  | Munop (Neg,e) -> condition e lif lelse
  | Mand (e1,e2) -> condition e1 (condition e2 lif lelse) lelse 
  | Mor (e1,e2) -> condition e1 lif (condition e2 lif lelse)
  | Munop (Slti n,e) ->
      let r = R.fresh () in
      expr r e (generate (
        if n = zero
          then Ubch (Bltz, r,lif,lelse)
          else Ubch (Blti n,r,lif,lelse)))
  | Munop (Sgti n,e) ->
      let r = R.fresh () in
      expr r e (generate (
        if n = zero
          then Ubch (Bgtz, r,lif,lelse)
          else Ubch (Bgti n,r,lif,lelse)))
  | Munop (Seqi n, e) ->
      let r = R.fresh () in
      expr r e (generate (
        if n = zero
          then Ubch (Beqz, r,lif,lelse)
          else Ubch (Beqi n,r,lif,lelse)))
  | Munop (Snei n, e) ->
      let r = R.fresh () in
      expr r e (generate (
        if n = zero
          then Ubch (Bnez, r,lif,lelse)
          else Ubch (Bnei n,r,lif,lelse)))
  | Mbinop (Seq,e1,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r1 e1 (
        expr r2 e2 (
          generate (Bbch (Beq,r1,r2,lif,lelse))))
  | Mbinop (Sne,e1,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r1 e1 (
        expr r2 e2 (
          generate (Bbch (Bne,r1,r2,lif,lelse))))
  | Mbinop (Slt,e1,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r1 e1 (
        expr r2 e2 (
          generate (Bbch (Blt,r1,r2,lif,lelse))))
  | Mbinop (Sle,e1,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r1 e1 (
        expr r2 e2 (
          generate (Bbch (Ble,r1,r2,lif,lelse))))
  | e ->
      let r = R.fresh () in
      expr r e (generate (Ubch (Beqz,r,lif,lelse)))

let expr_list = List.fold_right (fun e l -> expr (R.fresh()) e l)

let rec instr ret s exit1 dest1 = match s with
  | Nop -> dest1
  | Expr e -> expr (R.fresh ()) e dest1
  | If (e,i1,i2) ->
      condition e (instr ret i1 exit1 dest1) (instr ret i2 exit1 dest1)
  | While (e,i) ->
      let l = L.fresh () in
      let entry = condition e (instr ret i exit1 l) dest1 in
      graph := L.M.add l (Jump entry) !graph;
      entry
  | For (init,cond,inc,i) ->
      let l = L.fresh () in
      let iter  = expr_list inc l in
      let entry = condition cond (instr ret i exit1 iter) dest1 in
      let top = expr_list init entry in
      graph := L.M.add l (Jump top) !graph;
      top
  | Bloc il ->
      List.fold_right (fun s d -> instr ret s exit1 d) il dest1
  | Ret None -> exit1
  | Ret (Some e) -> expr (R.fresh()) e exit1

let arg_of_formal = function
  | R.R r -> R.R r
  | R.F ofs -> R.S ofs
  | R.S _ -> assert false

let fct {
  retsz=sz;
  fid=f;
  Iselect.formals=fml;
  Iselect.locals=lcl;
  locsz=a;
  Iselect.body=i} =
  init ();
  let ret = R.fresh () in
  let exit = L.fresh () in
  let lcl = Array.init lcl (fun _ -> R.fresh ()) in
  Array.iteri (Hashtbl.add locals) lcl;
  let entry = instr ret i exit exit in
  let fmls = Array.to_list (Array.sub lcl 0 fml) in
  {  
    ident=f;
    formals=fmls;
    locals=lcl;
    locsz=a;
    args=List.map arg_of_formal fmls;
    return=ret;
    entry=entry;
    exit=exit;
    body= !graph;
    (* putting some dummy values *)
    cr_saved=R.caller_saved;
    ce_saved=[];
    s_ident="";
  }

let rtl_of_is (f,v,data) =
  let f = List.map fct f in
  f,v,data

(**)
let reset () =
  Iselect.reset ();
  init ();

