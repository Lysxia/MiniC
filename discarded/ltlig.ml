open Rtl
open Ertl
open Ltlvlife

(* We will finally split structs and ints *)
type arcs = { pref:R.S.t ; intf:R.S.t }

type graph = arcs R.M.t

let make live instr =
  let g = ref R.M.empty in
  let add_pref r s =
    let e = try
      R.M.find r !g
    with Not_found -> {pref=R.S.empty;intf=R.S.empty} in
    g := R.M.add r {e with pref=R.S.add s e.pref} in
  let add_pref r s =
    add_pref r s;
    add_pref s r in
  let add_intf r s =
    let e = try
      R.M.find r !g
    with Not_found -> {pref=R.S.empty;intf=R.S.empty} in
    g := R.M.add r {e with intf=R.S.add s e.intf} in
  let add_intf r s =
    add_intf r s;
    add_intf s r in
  let check_instr def alive = function
    | Move (_,s) ->
        add_pref r s;
        List.iter (fun t -> if t<>s then add_intf def s) alive;
    | _ -> List.iter (add_intf def)  alive
  in
  let instr_list in_ =

