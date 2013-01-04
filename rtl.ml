open Iselect

module R = struct
  type t = int
  let free = ref 0
  let fresh () = incr free; !free
  module S = Set.Make(struct type t=int let compare=compare end)
end

module L = struct
  type t = int
  let free = ref 0
  let fresh () = incr free; !free
  module M = Map.Make(struct type t=int let compare=compare end)
end

(*****************)

type reg = R.t
type lab = L.t
type i32 = Int32.t

type u=munop
type b=mbinop
type ubranch =
  | Bgez | Bgezal | Blez | Bltz | Bltzal
  | Beqi of i32 | Beqzi | Blti of i32 | Bnei of i32
type bbranch = Beq | Bne | Blt | Ble

type instr =
  | Const of reg*i32        *lab
  | Move  of reg*reg        *lab
  | Unop  of reg*u*reg      *lab
  | Binop of reg*b*reg*reg  *lab
  | La    of reg*string     *lab
  | Addr  of reg*reg        *lab
  | Load  of reg*int*i32*reg*lab
  | Lw    of reg*i32*reg    *lab
  | Lb    of reg*i32*reg    *lab
  | Stor  of reg*reg*int*i32*reg*lab
  | Sw    of reg*reg*i32*reg    *lab
  | Sb    of reg*reg*i32*reg    *lab
  | Call  of reg*string*reg list*lab
  | Jump  of lab
  | Ubch  of ubranch*reg*lab*lab
  | Bbch  of bbranch*reg*reg*lab*lab

type graph = instr L.M.t

type fct = {
  ident:string;
  formals:reg list;
  return:reg;
  locals:R.S.t;
  entry:lab;
  exit:lab;
  body:graph;
}

(* As was done in class *)
let graph = ref L.M.empty
let locals = Hashtbl.create 14

let init () =
  graph := L.M.empty;
  Hashtbl.clear locals

let find_loc i =
  try
    Hashtbl.find locals i
  with
  | Not_found -> Printf.eprintf "Error %d\n%!" i;
      failwith "Error."

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
  | Maddr i -> generate (Addr (r,find_loc i,l))
  | Mload (sz,ofs,e) ->
      let r_ = R.fresh () in
      expr r_ e (generate (Load (r,sz,ofs,r_,l)))
  | Mlw (ofs,e) ->
      let r_ = R.fresh () in
      expr r_ e (generate (Lw (r,ofs,r_,l)))
  | Mlb (ofs,e) ->
      let r_ = R.fresh () in
      expr r_ e (generate (Lb (r,ofs,r_,l)))
  | Mstor (sz,e1,ofs,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r2 e2 (expr r1 e1 (generate (Stor (r,sz,r1,ofs,r2,l))))
  | Msw (e1,ofs,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r2 e2 (expr r1 e1 (generate (Sw (r,r1,ofs,r2,l))))
  | Msb (e1,ofs,e2) ->
      let r1 = R.fresh () in
      let r2 = R.fresh () in
      expr r2 e2 (expr r1 e1 (generate (Sb (r,r1,ofs,r2,l))))
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
  | Munop (Iselect.Move i,e) ->
      let r_ = R.fresh () in
      let ri = find_loc i in
      condition e (generate (Move (ri,r_,lif)))
                  (generate (Move (ri,r_,lelse)))
  | Munop (Muli zero,e) ->
      expr (R.fresh ()) e lelse
  | Mand (e1,e2) ->
      condition e1 (condition e2 lif lelse) lelse 
  | Mor (e1,e2) ->
      condition e1 lif (condition e2 lif lelse)
  | Munop (Neg,e) -> condition e lif lelse
  | Munop (Slti n,e) ->
      let r = R.fresh () in
      expr r e (generate (Ubch (Blti n,r,lif,lelse)))
  | Munop (Seqi n, e) ->
      let r = R.fresh () in
      expr r e (generate (Ubch (Beqi n,r,lif,lelse)))
  | Munop (Snei n, e) ->
      let r = R.fresh () in
      expr r e (generate (Ubch (Bnei n,r,lif,lelse)))
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
      expr r e (generate (Ubch (Beqi zero,r,lif,lelse)))

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

let fct {
  retsz=sz;
  fid=f;
  Iselect.formals=fml;
  Iselect.locals=lcl;
  locsz=a;
  Iselect.body=il} =
  init ();
  let ret = R.fresh () in
  let exit = L.fresh () in
  let lcl = Array.init lcl (fun _ -> R.fresh ()) in
  Array.iteri (Hashtbl.add locals) lcl;
  let formals = Array.to_list (Array.sub lcl 0 fml) in
  let entry =
    List.fold_right (fun i l -> instr ret i exit l) il exit in
  {
    ident=f;
    formals=formals;
    locals =Hashtbl.fold (fun _ r s -> R.S.add r s) locals R.S.empty;
    return = ret;
    entry = entry;
    exit = exit;
    body = !graph;
  }

let mk_graph (f,c,data) =
  List.map fct f,c,data
