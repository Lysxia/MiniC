(** Mini-C Compiler **)
(* Li-yao Xia *)

(* Instruction selection, taking inspiration from class *)

type munop =
  | Neg
  | Divi of Int32.t
  | Remi of Int32.t
  | Addi of Int32.t
  | Subi of Int32.t
  | Muli of Int32.t

  | Slti of Int32.t
  | Seqi of Int32.t
  | Snei of Int32.t
  | Sgti of Int32.t
  | Andi of Int32.t
  | Sltiu of Int32.t

  | Srl of int
  | Sll of int

type mbinop =
  | Add | Div | Mul | Sub | Rem | Seq | Sne | Slt | Sle | Sltu | Or

(* unsigned addu and subu do not overflow...
 * those are what we will really use *)

type expr =
  | Mconst  of Int32.t (* li *)
  | Munop   of munop*expr
  | Mbinop  of mbinop*expr*expr
  | Mla     of string
  | Maddr   of Typing.tident (*snd:size*)
  | Mload   of bool*int*Int32.t*expr (* for some size *)
  | Mstor   of bool*int*expr*Int32.t*expr
  | Mand    of expr*expr
  | Mor     of expr*expr
  | Mcall   of int*string*expr list
  | Mcall_addr of int*string*expr list

type instr =
  | Nop
  | Expr  of expr
  | If    of expr*instr*instr
  | While of expr*instr
  | For   of expr list*expr*expr list*instr
  | Bloc  of instr list
  | Ret   of expr option

type vdec = int*string

type data =
  | Str of Ast.str
  | Space of int

type fct = {
  retsz:int;
  retal:bool;
  fid:string;
  formals:int;
  locals:int;
  locsz:(bool*int) array;
  body:instr
}

(* Information about constructed types
 * size,alignment,fields offsets *)
val constr : (Typing.tident,int*bool*Int32.t array) Hashtbl.t

(* holds all global data (strings and global variables) *)
val data : (string*data) list ref

val reset : unit -> unit

(* Apply instruction selection to a function *)
val isfct : Typing.tfct -> fct

val gvars : Typing.tvdec list -> unit

(* Draw the structure/union fields layout *)
val isconstr : Typing.tconstr -> unit

val isprog : Typing.tfile -> fct list
