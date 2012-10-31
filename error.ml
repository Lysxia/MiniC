(** Error handling module **)

open Lexing

exception E of position*position*string

let prerr sp ep s =
  Printf.fprintf stderr
    "File \"%s\", line %d, characters %d-%d:\n%s\n%!"
    sp.pos_fname
    sp.pos_lnum
    (sp.pos_cnum-sp.pos_bol)
    (ep.pos_cnum-sp.pos_bol)
    s;
