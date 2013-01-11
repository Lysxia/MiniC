(** Mini-C Compiler **)
(* Li-yao Xia *)

open Iselect
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
  | SP -> "$sp"
  | ZERO -> "$zero"

(* printx(1)(l) prints an instruction name
 * with x registers,
 * (1 32 bit immediate)
 * (eventually a label) *)

let print3 h instr r s t =
  fprintf h "\t%s %s,%s,%s\n"
    instr (reg_string r) (reg_string s) (reg_string t)

let print2 h instr r s =
  fprintf h "\t%s %s,%s\n" instr (reg_string r) (reg_string s)

let print21 h instr r s n =
  fprintf h "\t%s %s,%s,%s\n"
    instr (reg_string r) (reg_string s) (to_string n)

let print1l h instr r l =
  fprintf h "\t%s %s,%s\n" instr (reg_string r) l

let print11l h instr r n l =
  fprintf h "\t%s %s,%s,%s\n" instr (reg_string r) (to_string n) l

let print2l h instr r s l =
  fprintf h "\t%s %s,%s,%s\n" instr (reg_string r) (reg_string s) l

(* memory access functions *)
let print_ma h instr r ofs s =
  fprintf h "\t%s %s,%d(%s)\n" instr (reg_string r) ofs (reg_string s)

let rec print_unop h u r s = match u with
  | Neg -> print2 h "neg" r s
  | Andi n -> print21 h "andi" r s n
  | Addi n -> print21 h "addiu" r s n
  | Subi n -> print21 h "subiu" r s n
  | Muli n -> print21 h "mul" r s n
  | Divi n -> print21 h "div" r s n
  | Remi n -> print21 h "rem" r s n
  | Slti n -> print21 h "slti" r s n
  | Sltiu n -> print21 h "sltiu" r s n
  | Sgti n -> print21 h "sgt" r s n
  | Seqi n -> print21 h "seq" r s n
  | Snei n -> print21 h "sne" r s n
  | Srl k -> fprintf h "\tsrl %s,%s,%d\n" (reg_string r) (reg_string s) k
  | Sll k -> fprintf h "\tsll %s,%s,%d\n" (reg_string r) (reg_string s) k

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

let print_ubrch h u r l = match u with
  | Bgtz -> print1l h "bgtz" r l
  | Bgtzal -> print1l h "bgtzal" r l
  | Bgez -> print1l h "bgez" r l
  | Bgezal -> print1l h "bgezal" r l
  | Blez -> print1l h "blez" r l
  | Bltz -> print1l h "bltz" r l
  | Bltzal -> print1l h "bltzal" r l
  | Beqzi -> print1l h "beqz" r l
  | Bnez -> print1l h "bnez" r l
  | Beqz -> print1l h "beqz" r l
  | Beqi n -> print11l h "beq" r n l
  | Blti n -> print11l h "blt" r n l
  | Bgti n -> print11l h "bgt" r n l
  | Bnei n -> print11l h "bne" r n l

let bb_string = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Ble -> "ble"

let rec print_text h = function
  | Nop -> ()
  | Comment s -> fprintf h "\t#%s\n" s
  | Concat (t1,t2) -> print_text h t1; print_text h t2
  | Li (r,n) -> fprintf h "\tli %s,%s\n" (reg_string r) (to_string n)
  | La (r,s) -> fprintf h "\tla %s,%s\n" (reg_string r) s
  | Move (r,s) -> print2 h "move" r s
  | Unop (r,u,s) -> print_unop h u r s
  | Binop (r,o,s,t) -> print3 h (binop_string o) r s t
  | Lw (r,ofs,t) -> print_ma h "lw" r ofs t
  | Lb (r,ofs,t) -> print_ma h "lbu" r ofs t
  | Sw (r,ofs,t) -> print_ma h "sw" r ofs t
  | Sb (r,ofs,t) -> print_ma h "sb" r ofs t
  | J l -> fprintf h "\tj %s\n" l
  | Jal l -> fprintf h "\tjal %s\n" l
  | Jr r -> fprintf h "\tjr %s\n" (reg_string r)
  | Ubch (u,r,l) -> print_ubrch h u r l
  | Bbch (b,r,s,l) -> print2l h (bb_string b) r s l
  | Syscall -> fprintf h "\tsyscall\n"
  | Label l -> fprintf h "%s:\n" l

let print_data h (s,x) =
  match x with
    | Space n -> fprintf h "%s:\n\t.space\t%d\n" s n
    | Str (Ast.Str u) -> fprintf h "%s:\n\t.asciiz\t\"%s\"\n" s u
    | Str (Ast.Ascii u) ->
        fprintf h "%s:\n\t.byte\t" s;
        for i = 0 to Array.length u-1 do
          fprintf h "%d," u.(i);
        done;
        fprintf h "0\n"

let print_prog h tast =
  let c,f,v = tast in
  let acc = ref None in
  let defrost = ref (fun () -> ()) in
  List.iter Iselect.isconstr c;
  Iselect.gvars v;
  List.iter (collect acc defrost) (List.map isfct f);
  if !recmain then !defrost ();
  fprintf h "\t.text\n";
  begin
    match !acc with
      | Some text -> print_text h text
      | None -> assert false
  end;
  Smap.iter (fun _ f -> print_text h f.body) !f_map;
  fprintf h "\t.data\n";
  List.iter (print_data h) !data
