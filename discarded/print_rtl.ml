open Iselect
open Rtl
open Print_ist
open Int32
open Format
open Ltlvlife

let ub_string = function
  | Bgtz -> "bgtz"
  | Bgtzal -> "bgtzal"
  | Bgez -> "bgez"
  | Bgezal -> "bgezal"
  | Blez -> "blez"
  | Bltz -> "bltz"
  | Bltzal -> "bltzal"
  | Beqzi -> "beqzi"
  | Bnez -> "bnez"
  | Beqz -> "beqz"
  | Beqi n -> "beqi("^to_string n^")"
  | Blti n -> "blti("^to_string n^")"
  | Bgti n -> "bgti("^to_string n^")"
  | Bnei n -> "bnei("^to_string n^")"

let bb_string = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Ble -> "ble"

let reg_string = function
  | R.R r -> "R"^string_of_int r
  | R.F r -> string_of_int r^"($fp)"
  | R.S r -> string_of_int r^"($sp)"

let rec print_rl ch = function
  | [] -> ()
  | [h] -> fprintf ch "#%s" (reg_string h)
  | h::t -> fprintf ch "#%s@ " (reg_string h);
      print_rl ch t

let rec print_reg_set h s =
  R.S.iter (fun r -> fprintf h "#%s@ " (reg_string r)) s

let rec print_instr h = function
  | Const (r,n,l) -> fprintf h "#%s <- %s" (reg_string r) (to_string n)
  | Move (r,s,l) ->
      fprintf h "#%s <- #%s" (reg_string r) (reg_string s)
  | Unop (r,u,s,l) -> fprintf h "#%s <- %s #%s"
      (reg_string r) (unop_string u) (reg_string s)
  | Binop (r,o,s,t,l) -> fprintf h "#%s <- %s #%s #%s"
      (reg_string r) (binop_string o) (reg_string s) (reg_string t)
  | La (r,x,l) -> fprintf h "#%s <- la %s" (reg_string r) x
  | Addr (r,s,l) ->
      fprintf h "#%s <- &#%s" (reg_string r) (reg_string s)
  | Load (r,_,sz,n,s,l) -> fprintf h "#%s <- Load(%d) %s(#%s)"
      (reg_string r) sz (to_string n) (reg_string s)
  | Stor (r,_,t,sz,n,s,l) -> fprintf h "#%s <- Store(%d) #%s %s(#%s)"
      (reg_string r) sz (reg_string t) (to_string n) (reg_string s)
  | Call (r,f,rl,l) -> fprintf h "#%s <- %s(%a)"
      (reg_string r) f print_rl rl
  | Jump l -> fprintf h "j"
  | Ubch (u,r,l,m) -> fprintf h "%s #%s |%d %d" (ub_string u) (reg_string r) l m
  | Bbch (b,r,s,l,m) -> fprintf h "%s #%s #%s |%d %d"
      (bb_string b) (reg_string r) (reg_string s) l m
  | ECall (f,l) -> fprintf h "[%s]" f
  | Syscall l -> fprintf h "syscall"
  | Alloc_frame l -> fprintf h "alloc_frame"
  | Free_frame l -> fprintf h "free_frame"
  | Return -> fprintf h "return"
  | ETCall (f) -> fprintf h "return [%s]" f
  | Label (s,l) -> fprintf h "lab:%s" s
  | Blok i -> print_block h i

and print_block h = function
  | [] -> ()
  | [i] -> fprintf h "%a" print_instr i;
  | i::t -> fprintf h "%a@\n" print_instr i;
      print_block h t

let rec print_int_list h = function
  | [] -> ()
  | [i] -> fprintf h "%d" i
  | i::t -> fprintf h "%d " i; print_int_list h t

let print_body h body =
  L.M.iter (fun l i ->
    fprintf h "%d > %a : %a@\n" l
      print_int_list (succ i)
      print_instr i) body

let print_instr_block h f ((succ,pred),io) =
  L.M.iter (fun l i ->
    let i_,o_ = L.M.find l io in
    fprintf h "%d < %a, > %a.@\nin [%a]@\nout [%a]@\n" l
    print_int_list (L.M.find l pred)
    print_int_list (L.M.find l succ)
    print_reg_set i_ print_reg_set o_;
    fprintf h "  @[%a@]@\n"
    print_instr i) f.body

let print_fct h f =
  fprintf h "#%s %s(%a)@\n  @[Entry: %d; Exit:%d@\n%a@]@\n"
    (reg_string f.return) f.ident print_rl f.formals f.entry f.exit
  print_body f.body

let print_blokfct h f =
  let succ_pred = build f in
  let io = in_out f succ_pred in
  fprintf h "#%s %s(%a)@\n  @[Entry: %d@\n%a@]@\n"
    (reg_string f.return) f.ident print_rl f.formals f.entry
    (fun h sp -> print_instr_block h f sp) (succ_pred,io);
