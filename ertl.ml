(* Arguments in $a0...$a3, then on stack by default.
 * Will be placed on stack :
   * Any additional argument when $a0..$a3 are used
   * Any variable which was referenced with &
   * Union and Structure when one of their fields is accessed into
   * Union and Structure when their size is > 4
  * (Accessing a U/S by name was converted into
  * dereferencing its address so it should be detected
  * only problem might be when your function only does return;
  * but then you don't expect anything to come out) *)
(* Result is returned in $v0 or on stack, same as above *)

open Rtl

let f_map = ref Smap.empty

let reset () =
  f_map := Smap.empty

let find f =
  try
    Smap.find f !f_map
  with
  | Not_found -> Printf.eprintf "%s\n%!" f; assert false

let rec set_arg fml_l arg_l l = match fml_l,arg_l with
  | [],[] -> l
  | f::fl,a::al -> set_arg fl al (generate (
      match f with
        | R.R _ -> Move (f,a,l)
        | R.F ofs -> Move (R.S ofs,a,l)
        | R.S _ -> assert false))
  | [],_ | _,[] -> assert false

let rec set_savers savers ce_saved l = match ce_saved with
  | [] -> l
  | (s,r)::t -> set_savers savers t (generate (
      Move (r,(List.assoc s savers),l)))

let instr savers exit e = match e with
  | Const _
  | Move _ | Unop _ | Binop _
  | La _ | Addr _
  | Load _ | Lw _ | Lb _
  | Stor _ | Sw _ | Sb _
  | Jump _ | Ubch _ | Bbch _ -> e
  | Call (r,f,arg,l) ->
      if f="sbrk"
        then Move (R.a0,List.hd arg,
          generate (Const (R.v0,Int32.of_int 9,
            generate (Syscall (generate (Move (r,R.v0,l)))))))
      else if f="putchar"
        then Move (R.a0,List.hd arg,
          generate (Const (R.v0,Int32.of_int 11,
            generate (Syscall l))))
      else if l = exit
        then begin
          let f_ = find f in
          let call = generate (ETCall f) in
          let args = set_arg f_.args arg call in
          let save = set_savers savers f_.ce_saved args in
          Jump save
          (* Will have to be careful with the output location *)
        end
      else begin
        let f_ = find f in
        let res = generate (Move (r,f_.return,l)) in
        let call = generate (ECall (f,res)) in
        Jump (set_arg f_.formals arg call)
      end
  | ECall _ | Syscall _ | Alloc_frame _ | Free_frame _ | Return
  | ETCall _ | Label _ | Blok _ ->
      assert false

let move r t l = generate (Move (t,r,l))

let fun_entry f savers fml entry =
  let l = generate (Label ("s_"^f,entry)) in
  let l = generate (Alloc_frame l) in
  List.fold_right (fun (r, t) l -> move r t l) savers l

let fun_exit savers return exit =
  let l = List.fold_right (fun (r, t) l -> move t r l) savers
    (generate Return) in
  let l = generate (Free_frame l) in
  graph := L.M.add exit (Jump l) !graph


let deffun f =
  f_map := Smap.add f.ident f !f_map;
  let savers = List.map (fun r -> r,R.fresh())
      (R.ra :: R.callee_saved) in
  L.M.iter
    (fun l i -> graph := L.M.add l (instr savers f.exit i) !graph) f.body;
  f.s_ident <- "s_"^f.ident;
  if f.ident <> "main"
    then f.ident <- "_"^f.ident;
  let entry = fun_entry f.ident savers f.formals f.entry in
  fun_exit savers f.return f.exit;
  f.body <- !graph;
  f.entry <- entry

let ertl_of_isf f =
  let f = Rtl.fct f in
  deffun f;
  f

let ertl_of_is (f,v,d) =
  let f = List.map ertl_of_isf f in
  f,v,d
