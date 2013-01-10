(* We now produce code straight out of instruction selection *) (*
Calling convention :
  * ALL arguments go on the stack Return values are stored either in $v0
  * or on the stack : Values with size > 4 go on the stack at 0($fp)
  * Others go in $v0 *)
(* All local variables are stored on the stack too *)

open Int32
open Iselect

type reg = V0 | A0 | A1 | A2 | A3 | FP | SP | RA | ZERO

type ubranch =
  | Bgtz | Bgtzal | Bgez | Bgezal | Blez | Bltz | Bltzal | Beqz | Bnez
  | Beqi of t | Beqzi | Blti of t | Bgti of t | Bnei of t
type bbranch = Beq | Bne | Blt | Ble

type label = string

type text =
  | Nop
  | Li of reg*t
  | La of reg*string
  | Move of reg*reg
  | Unop of reg*munop*reg
  | Binop of reg*mbinop*reg*reg
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

type data =
  | Asciiz of label*string

type fct =
  {
    ident:string;
    fmlpos:int list;
    fmlsz:int list;
    fmlal:bool list;
    outsz:int
  }

let (++) c1 c2 = match c1,c2 with
  | Nop,_ -> c2
  | _,Nop -> c1
  | _,_ -> Concat (c1,c2)

let free = ref 0

let fresh () = incr free; string_of_int !free

let four = of_int 4

let f_map:fct Smap.t ref = ref Smap.empty

(**)
let load a s ofs reg sp =
  if a
    then
      if s=4
        then Lw (A0,ofs,reg)
        else begin
          let ls = ref Nop in
          for i = 0 to s/4-1 do
            ls := Lw (A2,ofs+s*i,reg)
              ++ Sw (A2,sp+s*i,SP)
              ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if s > 4
        then
          for i=0 to s-1 do
            ls := Lb (A2,ofs+i,reg)
              ++ Sb (A2,sp+i,SP)
              ++ !ls
          done
        else
          for i=0 to s-1 do
            ls := Unop (A0,Sll 4,A0)
              ++ Lb (A2,ofs+i,reg)
              ++ Binop (A0,Or,A0,A2)
              ++ !ls
          done;
        !ls
    end

let store a s ofs reg sp =
  if s=4
    then
      if a
        then Sw (A0,ofs,reg)
        else begin
          let ls = ref Nop in
          for i=0 to s-1 do
            ls := Lb (A2,sp+i,SP)
            ++ Sb (A2,ofs+i,reg)
            ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if a (* Value to store is on stack *)
        then begin (* Value to be stored is on stack *)
          for i = 0 to s/4-1 do
            ls := Lw (A2,ofs+s*i,SP)
              ++ Sw (A2,sp+s*i,reg)
              ++ !ls
          done
        end
        else
          for i=0 to s-1 do
            ls := Unop (A0,Sll 4,A0)
              ++ Sb (A2,ofs+i,reg)
              ++ Binop (A0,Or,A0,A2)
              ++ !ls
          done;
        !ls
    end

let branching brch_instr l1 l2 b1 b2 =
  brch_instr
  ++ b1
  ++ J l2
  ++ Label l1
  ++ b2
  ++ Label l2

(* e1 and e2 are int or char typed expressions*)
(* stores the outputs in A0 and A1 *)
let rec byte_pair arg_pos sp e1 e2 =
  expr arg_pos sp e1
  ++ store true 4 sp SP sp
  ++ expr arg_pos (sp-4) e2
  ++ Move (A1,A0)
  ++ load true 4 sp SP sp

(* convert expr to a sequence of mips instructions *)
(* arg_pos gives the offset of each argument relatively to $fp
 * Result is put on stack if its size is > 4, $a0 otherwise *)
and expr arg_pos sp =
  let store_args f el =
    let f = Smap.find f !f_map in
    let rec store_args sp el pos al sz = match el,pos,al,sz with
      | [],_,_,_ -> Nop
      | e::el,p::pos,a::al,s::sz ->
          (expr arg_pos sp e)
          ++(if s>4 then Nop else store a s sp SP sp)
          ++store_args (sp-s) el pos al sz
      | _,_,_,_ -> assert false
    in
    store_args 0 el f.fmlpos f.fmlal f.fmlsz
  in function
  | Mconst n -> Li (A0,n)
  | Mla s -> La (A0,s)
  (*| Mload (a,s,n,Maddr i) ->
    let ofs = add n (of_int arg_pos.(i)) in
    let sp = sp - ((s+3)/4)*4 in
    load a s ofs FP sp*)
  | Mload (a,s,n,e) ->
      expr arg_pos sp e
      ++ Move (A1,A0)
      ++ load a s (to_int n) A1 (sp-s)
  | Mstor (a,s,e,n,f) ->
    expr arg_pos sp f
    ++ Sw (A0,sp,SP)
    ++ expr arg_pos (sp-4) e
    ++ Lw (A2,sp,SP)
    ++ store a s (to_int n) A1 (sp-s)
  | Maddr i -> Unop (A0,Addi (of_int arg_pos.(i)),FP)
  | Mcall (s,f,el) ->
      Unop (SP,Addi (of_int sp),SP)
      ++ store_args f el
      ++ Jal f
      ++ Unop (SP,Subi (of_int sp),SP)
  | Munop (u,e) -> (expr arg_pos sp e)++Unop(A0,u,A0)
  | Mbinop (o,e1,e2) ->
      byte_pair arg_pos sp e1 e2
      ++ Binop (A0,o,A0,A1)
  | Mand (e1,e2) ->
      condition arg_pos sp e1
        (expr arg_pos sp e2 ++ Binop (A0,Sltu,ZERO,A0))
        (Binop (A0,Sltu,ZERO,A0))
  | Mor (e1,e2) ->
      condition arg_pos sp e1 (Binop (A0,Sltu,ZERO,A0))
        (expr arg_pos sp e2 ++ (Binop (A0,Sltu,ZERO,A0)))

and condition arg sp e b1 b2 = match e with
  | Mconst n -> if n=zero then b2 else b1
  | Mand (e1,e2) ->
      condition arg sp e1 (condition arg sp e2 b1 b2) b2
  | Mor (e1,e2) ->
      condition arg sp e1 b1 (condition arg sp e2 b1 b2)
  | Munop (Neg,e) ->
      condition arg sp e b1 b2
  | Munop (Muli zero,e) ->
      expr arg sp e ++ b2
  | e ->
      let l1 = "b"^fresh () in
      let l2 = l1^"_" in
      branching (branch arg sp e l2) l1 l2 b1 b2

and branch arg_pos sp e l = match e with
  | Munop (Slti n,e) ->
      expr arg_pos sp e
      ++ (if n = zero
            then Ubch (Bltz,A0,l)
            else Ubch (Blti n,A0,l))
  | Munop (Sgti n,e) ->
      expr arg_pos sp e
      ++ (if n = zero
            then Ubch (Bgtz,A0,l)
            else Ubch (Bgti n,A0,l))
  | Munop (Seqi n,e) ->
      expr arg_pos sp e
      ++ (if n = zero
            then Ubch (Beqz,A0,l)
            else Ubch (Beqi n,A0,l))
  | Munop (Snei n,e) ->
      expr arg_pos sp e
      ++ (if n = zero
            then Ubch (Bnez,A0,l)
            else Ubch (Bnei n,A0,l))
  | Mbinop (Seq,e1,e2) ->
      byte_pair arg_pos sp e1 e2
      ++ Bbch (Beq,A0,A1,l)
  | Mbinop (Sne,e1,e2) ->
      byte_pair arg_pos sp e1 e2
      ++ Bbch (Bne,A0,A1,l)
  | Mbinop (Slt,e1,e2) ->
      byte_pair arg_pos sp e1 e2
      ++ Bbch (Blt,A0,A1,l)
  | Mbinop (Sle,e1,e2) ->
      byte_pair arg_pos sp e1 e2
      ++ Bbch (Ble,A0,A1,l)
  | e ->
      expr arg_pos sp e
      ++ Ubch (Beqz,A0,l)

      (*
let rec instr arg quit sp = function
  | Iselect.Nop -> Nop
  | Expr e -> expr arg sp e
  | If (e,i1,i2) ->
      condition arg sp e
        (instr arg quit sp i1) (instr arg sp i2)
  | While (e,i) ->
      let l = "while"^fresh () in
      let l_ = l^"_" in
      J l_
      ++ Label l
      ++ instr arg_pos quit sp i
      ++ Label l_
      ++ branch arg_pos l
  | *)
