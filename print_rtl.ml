open Iselect
open Rtl
open Print_ist
open Int32
open Format

let ub_string = function
  | Bgez -> "bgez"
  | Bgezal -> "bgezal"
  | Blez -> "blez"
  | Bltz -> "bltz"
  | Bltzal -> "bltzal"
  | Beqzi -> "beqzi"
  | Beqi n -> "beqi("^to_string n^")"
  | Blti n -> "blti("^to_string n^")"
  | Bnei n -> "bnei("^to_string n^")"

let bb_string = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Ble -> "ble"

let rec print_rl ch = function
  | [] -> ()
  | [h] -> fprintf ch "#%d" h
  | h::t -> fprintf ch "#%d " h;
      print_rl ch t

let print_instr h = function
  | Const (r,n,l) -> fprintf h "#%d <- %s\t|%d" r (to_string n) l
  | Move (r,s,l) -> fprintf h "#%d <- #%d\t|%d" r s l
  | Unop (r,u,s,l) -> fprintf h "#%d <- %s #%d\t|%d" r (unop_string u) s l
  | Binop (r,o,s,t,l) -> fprintf h "#%d <- %s #%d #%d\t|%d"
      r (binop_string o) s t l
  | La (r,s,l) -> fprintf h "#%d <- %s\t|%d" r s l
  | Addr (r,s,l) -> fprintf h "#%d <- &#%d\t|%d" r s l
  | Load (r,sz,n,s,l) -> fprintf h "#%d <- Load(%d) %s(#%d)\t|%d"
      r sz (to_string n) s l
  | Lw (r,n,s,l)
  | Lb (r,n,s,l) -> fprintf h "#%d <- l- %s(#%d)\t|%d" r (to_string n) s l
  | Stor (r,t,sz,n,s,l) -> fprintf h "#%d <- Store(%d) #%d %s(#%d)\t|%d"
      r sz t (to_string n) s l
  | Sw (r,t,n,s,l)
  | Sb (r,t,n,s,l) -> fprintf h "#%d <- s- #%d %s(#%d)\t|%d" r t (to_string n) s l
  | Call (r,f,rl,l) -> fprintf h "#%d <- %s(%a)\t|%d" r f print_rl rl l
  | Jump l -> fprintf h "|%d" l
  | Ubch (u,r,l,m) -> fprintf h "%s %d |%d %d" (ub_string u) r l m
  | Bbch (b,r,s,l,m) -> fprintf h "%s %d %d |%d %d" (bb_string b) r s l m

let print_fct h {
  ident=f;
  formals=formals;
  locals=_;
  return=ret;
  entry=entry;
  exit=exit;
  body=body;
  } =
    fprintf h "#%d %s(%a)@\n  @[Start: %d@\n"
      ret f print_rl formals entry;
    L.M.iter (fun l i -> fprintf h "%d : %a@\n" l print_instr i) body;
    fprintf h "@]@."


