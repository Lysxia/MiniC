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

let rec set_arg fml_l arg_l l = match fml_l,arg_l with
  | [],[] -> l
  | f::fl,a::al -> set_arg fl al (generate (
      match f with
        | R.R _ -> Move (f,a,l)
        | R.F ofs -> Move (R.S ofs,a,l)
        | R.S _ -> assert false))
  | [],_ | _,[] -> assert false

let rec set_arg_f fml_l arg_l l = match fml_l,arg_l with
  | [],[] -> l
  | f::fl,a::al -> set_arg_f fl al (generate (
      match f with
        | R.R _ | R.F _ -> Move (f,a,l)
        | R.S _ -> assert false))
  | [],_ | _,[] -> assert false

let instr exit e = match e with
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
            generate (Syscall l))))
      else if f="putchar"
        then Move (R.a0,List.hd arg,
          generate (Const (R.v0,Int32.of_int 11,
            generate (Syscall l))))
      else if l = exit
        then begin
          let f_ = Smap.find f !f_map in
          let call = generate (ETCall f) in
          L.M.find (set_arg_f f_.formals arg call) !graph
          (* Careful with the output location *)
        end
      else begin
        let f_ = Smap.find f !f_map in
        let res = generate (Move (r,f_.return,l)) in
        let call = generate (ECall (f,res)) in
        L.M.find (set_arg f_.formals arg call) !graph
      end
  | ECall _ | Syscall _ | Alloc_frame _ | Free_frame _ | Return
  | ETCall _ | Label _ ->
      assert false

let fun_entry f savers fml entry =
  let l = generate (Label ("s_"^f,entry)) in
  let l = List.fold right (fun (t, r) l -> move r t l) savers l in
  generate (Alloc_frame l)

let fun_exit savers return exit =
  let l = generate (Edelete frame (generate Ereturn)) in
  let l = List.fold right (fun (t, r) l -> move t r l) savers l in
  let l = move retr Register.result l in
    graph := Label.M.add exitl (Egoto l) !graph


let deffun f =
  graph := L.M.map (instr f.exit) f.body;
  let savers = List.map (fun r -> r,R.fresh())
      (R.ra :: R.callee_saved) in
  f_map := Smap.add f.ident f !f_map;
  f.s_ident <- "s_"^f.ident;
  if f.ident <> "main"
    then f.ident <- "_"^f.ident;
  let entry = fun_entry f.ident savers f.formals f.entry in
  let body = fun_exit savers f.return f.exit in

let ertl_of_isf f = deffun (Rtl.fct f)

let ertl_of_is (f,v,d) =
  let f = List.map ertl_of_isf f in
  f,v,d
