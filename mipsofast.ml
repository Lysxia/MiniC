type reg = string
(*
  | A  of int (*0-3*)
  | V  of int (*0-1*)
  | T  of int (*0-9*)
  | S  of int (*0-7*)
  | SP
  | FP
  | RA
  | GP*)

type mips =
  | Li    of reg*Int32.t
  | La    of reg*string
  | In    of string*reg*reg (* move,neg,abs,not *)
  | Op    of string*reg*reg*reg
      (* add,sub,mul,div,and,or,
       * sllv,srav,srlv,rol,ror,
       * slt,sltu,sle,sleu,sgt,sgtu,sge,sgeu... *)
  | Opi   of string*reg*reg*Int32.t
      (* addi,andi,ori,
       * sll,srq,srl,rol,ror,
       * slti,sltiu,sle,sleu,sgt,sgtu,sge,sgeu... *)
  | Mem   of string*reg*int*reg (* Offset on 16 signed bits *)
