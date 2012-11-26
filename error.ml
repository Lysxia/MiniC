(** Error handling module **)

open Lexing

exception E of position*position*string

let prerr name sp ep s =
  if sp.pos_cnum = ep.pos_cnum
    then Printf.fprintf stdout
      "File \"%s\", line %d, character %d:\n%s\n%!"
      name sp.pos_lnum
      (sp.pos_cnum-sp.pos_bol)
      s
  else if sp.pos_lnum = ep.pos_lnum
    then Printf.fprintf stdout
      "File \"%s\", line %d, characters %d-%d:\n%s\n%!"
      name sp.pos_lnum
      (sp.pos_cnum-sp.pos_bol)
      (ep.pos_cnum-sp.pos_bol)
      s
    else Printf.fprintf stdout
      "File \"%s\", lines %d-%d, characters %d-%d:\n%s\n%!"
      name sp.pos_lnum ep.pos_lnum
      (sp.pos_cnum-sp.pos_bol)
      (ep.pos_cnum-ep.pos_bol)
      s

let prgerr name s =
  Printf.fprintf stdout
    "File \"%s\", line 0, character 0:\n%s\n%!" name s

let syntax name lexbuf =
  prerr name (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)
    "Syntax error"
