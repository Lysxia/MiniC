(* ltlvlife : Variable lifetime,
 * ltlig : interference graph *)

open Rtl
open Ertl

(* As was suggested we will fusion successive instructions *)
type block = instr list

let rec last = function
  | [] -> invalid_arg "empty list"
  | [x] -> x
  | _::t -> last t

(* Successor(s) of an instruction *)
let rec succ = function
  | Const (_,_,l) | Move (_,_,l) | Unop (_,_,_,l)
  | Binop (_,_,_,_,l) | La (_,_,l) | Addr (_,_,l)
  | Load (_,_,_,_,l) | Lw (_,_,_,l) | Lb (_,_,_,l)
  | Stor (_,_,_,_,_,l) | Sw (_,_,_,_,l) | Sb (_,_,_,_,l)
  | Jump l | Syscall l | Alloc_frame l | Free_frame l
  | Call (_,_,_,l) | ECall (_,l) | Label (_,l) -> [l]
  | Ubch (_,_,l,m) | Bbch (_,_,_,l,m) -> [l;m]
  | ETCall _ | Return -> []
  | Blok il -> succ (last il)

(*(* Reachable nodes from entry (using intermediate basic block representation)
 * then complete the transformation *)
let reach bgraph entry =
  let q = Stack.create () in
  let ls = ref [] in
  let visited = ref L.M.empty in
  let push l =
    if not (L.M.mem l !visited)
      then begin 
        Stack.push l q;
        visited := L.M.add l () !visited;
      end in
  let visit l =
    let block = L.M.find l bgraph in
    ls := (l,Blok block)::!ls;
    List.iter push (succ (last block))
  in
  push entry;
  while not (Stack.is_empty q) do
    visit (Stack.pop q)
  done;
  List.rev !ls,
  List.fold_left (fun m (l,b) -> L.M.add l b m)
    L.M.empty !ls
*)

(* Turning graph into a basic block representation *)
let build f =
  let graph = f.body in
  let pred_count = ref (L.M.map (fun _ -> 0,[]) graph) in
  let incr l x =
    pred_count :=
      L.M.add l 
        (let k,pred=L.M.find l !pred_count in k+1,x::pred) !pred_count in
  let incr_count k i = List.iter (fun l -> incr l k) (succ i) in
  L.M.iter incr_count graph;
  let block_g = ref L.M.empty in
  let predc l = fst (L.M.find l !pred_count) in
  let pred = ref L.M.empty in
  let succ = ref L.M.empty in
  (* Beginning at label l find reachable nodes and build bb representation *)
  let rec build l0 acc l =
    let i = L.M.find l graph in
    match succ i with
      | [l] when predc l=1 -> build l0 (i::acc) l
      | ls -> block_g := L.M.add l0 (Blok (List.rev (i::acc))) !block_g;
          succ := L.M.add l0 ls !succ;
          List.iter (fun l ->
            pred := L.M.add l (l0::find_p l) !pred;
            if not (L.M.mem l !block_g)
              then build l [] l) ls
  in
  build f.entry [] f.entry;
  f.body <- !block_g;
  !succ,!pred

(* When we know an instruction is a block, output the corresponding list *)
let descblok = function
  | Blok b -> b
  | _ -> assert false

(* As is done in class, defined and used variables in each node *)
let def_use f =
  let add = List.fold_left (fun s x -> R.S.add x s) in
  let def_use1 = function
    | Const (r,_,_) | La (r,_,_) -> [r],[]
    | Addr (r,_,_) -> [],[]
    | Move (r,s,_) | Unop (r,_,s,_) | Load (r,_,_,s,_)
    | Lw (r,_,s,_) | Lb (r,_,s,_) -> [r],[s]
    | Binop (r,_,s,t,_) | Stor (r,s,_,_,t,_)
    | Sw (r,s,_,t,_) | Sb (r,s,_,t,_) -> [r],[s;t]
    | Ubch (_,r,_,_) -> [],[r]
    | Bbch (_,r,s,_,_) -> [],[r;s]
    | Jump _ | Alloc_frame _ | Label (_,_) | Free_frame _ -> [],[]
    | Syscall _ -> [R.v0],[R.a0;R.v0]
    | ECall (f,_) -> let f = find f in f.cr_saved,f.formals
    | ETCall f -> let f = find f in [],f.args
    | Return -> [],f.return::R.ra::R.callee_saved
    | Call _ | Blok _ -> assert false in
  let rec def_use d u = function
    | h::t -> let d_,u_ = def_use1 h in
        let u_ = List.filter (fun x -> R.S.mem x d) u_ in
        def_use (add d d_) (add u u_) t
    | [] -> d,u in
  L.M.map (fun b -> def_use R.S.empty R.S.empty (descblok b)) f.body

let succ_map = L.M.map (succ)

let in_out f pred succ =
  let def_use = def_use f in
  let def_use l = L.M.find l def_use in
  let fixit = ref (L.M.map (fun l _ -> add l; R.S.empty,R.S.empty) def_use) in
  let io l = L.M.find l !fixit in
  let add l x = L.M.add l x !fixit in
  let unstable = ref L.M.empty in
  let add x = unstable := L.M.add x () !unstable in
  (* A heuristic to try beginning from the bottom of the graph *)
  let take () = let k = L.M.min_binding !unstable in L.M.remove k !unstable in
  let succ_map = succ_map f.body in
  let succ l = L.M.find l succ_map in
  while not (L.M.is_empty !unstable) do
    let l = take () in
    let d,u = def_use l in
    let i_,_ = io l in
    let o = List.fold_left
      (fun s l -> R.S.union s (fst (io l))) R.S.empty (succ l) in
    let i = R.S.union u (R.S.diff o d) in
    if not (R.S.equal i i_) then
