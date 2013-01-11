(** Mini-C Compiler **)
(* Li-yao Xia *)

(* We now produce code straight out of instruction selection *)
(* Calling convention :
  * ALL arguments go on the stack Return values are stored
  * either in $a0 or on the stack :
    * Values with size > 4 go on the stack at 0($fp)
    * Others go in $a0 *)
(* All local variables are stored on the stack too *)
(* Intermediate results are put on stack *)

(* The computed value of a (sub-)expression 
 * is put in $a0 if its size does not exceed 4 bytes
 * else it is put on stack *)

(*****************************************)
(* arg1 <- $fp
 * arg2
 * ...
 * local1
 * ...
 * intermediate1   <- $sp
 * ...
 * ...
 * 4 empty bytes <- 'sp'($sp)
 * ...
 *)
(*****************************************)

(* Two arguments frequently appear in those functions *)
(* argpos (or arg) gives the position of local variables relative to $fp *)
(* sp is the current stack top position relative to $sp *)
(* it points to 4 free bytes (not necessarily aligned)
 * 'sp+3'($sp) 'sp+2'($sp) 'sp+1'($sp) 'sp'($sp)  *)

type reg = V0 | A0 | A1 | A2 | A3 | FP | SP | RA | ZERO

type ubranch =
  | Bgtz | Bgtzal | Bgez | Bgezal | Blez | Bltz | Bltzal | Beqz | Bnez
  | Beqi of Int32.t | Beqzi
  | Blti of Int32.t | Bgti of Int32.t | Bnei of Int32.t
type bbranch = Beq | Bne | Blt | Ble

type label = string

type text =
  | Nop
  | Comment of string
  | Li of reg*Int32.t
  | La of reg*string
  | Move of reg*reg
  | Unop of reg*Iselect.munop*reg
  | Binop of reg*Iselect.mbinop*reg*reg
  | Lw of reg*int*reg
  | Lb of reg*int*reg
  | Sw of reg*int*reg
  | Sb of reg*int*reg
  | J of label
  | Jal of label
  | Jr of reg
  | Ubch of ubranch*reg*label
  | Bbch of bbranch*reg*reg*label
  | Syscall
  | Label of label
  | Concat of text*text

type data = (string*Ast.str list)

type argpos = int array

type fct =
  {
    ident:string;
    argpos:int list;
    argsz:int list;
    argal:bool list;
    rsz:int;
    ral:bool;
    mutable body:text
  }

(* Resets the compiler so that we can compile a new file *)  
val reset : unit -> unit

(* Compile f if f is not main *)
(* in the case of main, delay its compilation as a standard function
  * because it is unlikely to be used as such
  * and generate specialized code *)
val collect : text option ref -> (unit -> unit) ref -> Iselect.fct -> unit

(*****************************)

(* Stores compiled functions *)
val f_map : fct Smap.t ref

(* true if there are calls to main *)
val recmain : bool ref

(* load a s ofs r sp *)
(* Load ofs(r) into $a0 or stack when s>4 *)
(* a : alignment of value to store
 * s : size of value *)
val load : bool -> int -> int -> reg -> int -> text

(* store a s ofs r sp *)
(* Store last calculated value which is saved either in $a0
 * or pushed on top of stack *)
val store : bool -> int -> int -> reg -> int -> text

(* branching brch l1 l2 b1 b2 *)
(* brch is an instruction which branches to l1
 * which corresponds to b1
 * if it does not branch, b2 is to be executed
 * basically, if brch then l1 (execute b1) else l2 (execute b2) *)
val branching : text -> label -> label -> text -> text -> text

(* word_pair argpos sp e1 e2 *)
(* e1 and e2 are int or char typed expressions
 * compute e2 then e1 and put the values in $a1 and $a0 *)
val word_pair : argpos -> int -> Iselect.expr -> Iselect.expr -> text

(* expr argpos sp e *)
(* convert expr to a sequence of MIPS instructions *)
(* compute e and store the result either in $a0 or stack *)
val expr : argpos -> int -> Iselect.expr -> text

(* condition arg sp e b1 b2 *)
(* translates to if (e) then b1 else b2 *)
val condition : argpos -> int -> Iselect.expr -> text -> text -> text

(* branch argpos sp e l *)
(* if (e) branch to l *)
val branch : argpos -> int -> Iselect.expr -> label -> text

(* instr arg (set_return,exit) *)
(* translate instruction to MIPS instructions *)
val instr : argpos -> text*text -> Iselect.instr -> text

(* compile a function and store it in the map *)
val fct : Iselect.fct -> unit
