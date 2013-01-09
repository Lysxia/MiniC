(* We now produce code straight out of instruction selection *)
(* Calling convention :
  * ALL arguments go on the stack
  * Return values are stored either in $v0 or on the stack :
    * Values with size > 4 go on the stack at 0($fp)
    * Others go in $v0 *)
(* All local variables are stored on the stack too *)

open Iselect

type reg = V0 | A0 | A1 | FP | SP | RA | ZERO

type ubranch =
  | Bgtz | Bgtzal | Bgez | Bgezal | Blez | Bltz | Bltzal | Beqz | Bnez
  | Beqi of i32 | Beqzi | Blti of i32 | Bgti of i32 | Bnei of i32
type bbranch = Beq | Bne | Blt | Ble

type label = string

type text =
  | Li of reg*Int32.t
  | La of reg*string
  | Unop of reg*munop*reg
  | Binop of reg*mbinop*reg*reg
  | Lw of reg*int*reg
  | Lb of reg*int*reg
  | Sw of reg*int*reg
  | Sb of reg*int*reg
  | Jal of label
  | Jr of reg
  | Ubch of ubranch*reg*label
  | Bbch of bbranch*reg*reg*label
  | Syscall
  | Label of label

type txt_list =
  | L of mips_text list
  | Concat of txt_list*txt_list

type data =
  | Asciiz of label*string

let (++) c1 c2 = Concat (c1,c2)

let set r = r := true

let save_ra = ref false
let save_fp = ref false

(* convert expr to a sequence of mips instructions *)
(* arg_loc gives the size and offset of each argument relatively to fp *)
let rec expr arg_loc fp = function
  | 
