open Mips
open Int32
open Printf

let reg_string = function
  | V0 -> "$v0"
  | A0 -> "$a0"
  | A1 -> "$a1"
  | A2 -> "$a2"
  | A3 -> "$a3"
  | FP -> "$fp"
  | RA -> "$ra"
  | ZERO -> "$zero"

(* printxy(l) prints an instruction name
 * with x registers and y (0 or 1) 32 bit immediate
 * and eventually a label *)

let print3 h instr r s t =
  fprintf h "\t%s %s,%s,%s\n"
    instr (reg_string r) (reg_string s) (reg_string t)

let print2 h instr r s =
  fprintf h "\t%s %s,%s\n" instr (reg_string r) (reg_string s)

let print21 h instr r s n =
  fprintf h "\t%s %s,%s,%s\n"
    instr (reg_string r) (reg_string s) (to_string n)

let print11l h instr r n l =
  fprintf h "\t%s %s,%s,%s\n" instr (reg_string r) (to_string n) l

let print2l h instr r s l =
  fprintf h "\t%s %s,%s,%s\n" instr (reg_string r) (reg_string s) l

(* memory access functions *)
let print_ma h instr r ofs s =
  fprintf h "\t%s %s,%d(%s)\n" instr (reg_string r) ofs (reg_string s)

let rec print_unop h u r s = match u with
  | Neg -> print2 "neg" r s
  | Andi n -> print21 h "andi" r s (to_string n)
  | Addi n -> print21 h "addiu" r s (to_string n)
  | Subi n -> print21 h "subiu" r s (to_string n)
  | Muli n -> print21 h "muli" r s (to_string n)
  | Divi n -> print21 h "div" r s (to_string n)
  | Remi n -> print21 h "rem" r s (to_string n)
  | Slti n -> print21 h "slti" r s (to_string n)
  | Sltiu n -> print21 h "sltiu" r s (to_string n)
  | Sgti n -> print21 h "sgt" r s (to_string n)
  | Seqi n -> print21 h "seq" r s (to_string n)
  | Snei n -> print21 h "sne" r s (to_string n)
  | Sll k -> fprintf h "sll %s,%s,%d\n" r s k

let rec binop_string = function
  | Add -> "add"
  | Div -> "div"
  | Mul -> "mul"
  | Sub -> "sub"
  | Rem -> "rem"
  | Slt -> "slt"
  | Sle -> "sle"
  | Seq -> "seq"
  | Sne -> "sne"
  | Sltu -> "sltu"
  | Or -> "or"

let rec print_text h = function
  | Concat (t1,t2) -> print_text t1; print_text t2;
  | Nop -> ()
  | Li (r,n) -> print2 h "li" r n
  | La (r,s) -> fprintf h "\tla %s,%s\n" (reg_string r) s
  | Move (r,s) -> print2 h "move" r s
  | Unop (r,u,s) -> print_unop h u r s
  | Binop (r,o,s,t) -> print3 h (binop_string o) r s t
  | Lw (r,ofs,t) -> print_ma h "lw" r ofs t
  | Lb (r,ofs,t) -> print_ma h "lb" r ofs t
  | Sw (r,ofs,t) -> print_ma h "sw" r ofs t
  | Sb (r,ofs,t) -> print_ma h "sb" r ofs t
  | J l -> fprintf h "\tj %s\n" l
  | Jal l -> fprintf h "\tjal %s\n" l
  | Jr r -> fprintf h "\tjr %s\n" (reg_string r)
  | 
