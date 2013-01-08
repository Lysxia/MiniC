open Rtl
open Ertl

let reg_sz f =
  (* essentially finding out too big structures
   * to be allocated in a register *)
  let reg_dep = ref R.M.empty in
  let reg_sz = ref f.locsz in
  let add_dep r s = reg_dep := R.M.add r s !reg_dep in
  let add_sz r sz = reg_sz := R.M.add r sz !reg_sz in
  let reg_sz r = R.M.find r !reg_sz in
  let rec find_dep = function
    | Const 
     
