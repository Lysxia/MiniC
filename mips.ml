(* We now produce code straight out of instruction selection *) (*
Calling convention :
  * ALL arguments go on the stack Return values are stored either in $v0
  * or on the stack : Values with size > 4 go on the stack at 0($fp)
  * Others go in $v0 *)
(* All local variables are stored on the stack too *)

open Int32 open Iselect

type reg = V0 | A0 | A1 | A2 | A3 | FP | SP | RA | ZERO

type ubranch =
  | Bgtz | Bgtzal | Bgez | Bgezal | Blez | Bltz | Bltzal | Beqz | Bnez
  | Beqi of i32 | Beqzi | Blti of i32 | Bgti of i32 | Bnei of i32
type bbranch = Beq | Bne | Blt | Ble

type label = string

type text =
  | Nop
  | Li of reg*t
  | La of reg*string
  | Move of reg*reg
  | Unop of reg*munop*reg
  | Binop of reg*mbinop*reg*reg
  | Lw of reg*t*reg
  | Lb of reg*t*reg
  | Sw of reg*t*reg
  | Sb of reg*t*reg
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

let (++) c1 c2 = match c1,c2 with
  | Nop,_ -> c2
  | _,Nop -> c1
  | _,_ -> Concat (c1,c2)

let free = ref 0

let fresh () = incr free; "L"^string_of_int !free

let four = of_int 4

(**)
let load a s ofs reg sp =
  if a
    then
      if s=4
        then Lw (A0,ofs,reg)
        else begin
          let ls = ref Nop in
          for i = 0 to s/4-1 do
            ls := Lw (A2,add ofs (mul four (of_int i)),reg)
              ++ Sw (A2,add sp (mul four (of_int i)),SP)
              ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if s > 4
        then
          for i=0 to s-1 do
            ls := Lb (A2,add ofs (of_int i),reg)
              ++ Sb (A2,add sp (of_int i),SP)
              ++ !ls
          done
        else
          for i=0 to s-1 do
            ls := Unop (A0,Sll 4,A0)
              ++ Lb (A2,add ofs (of_int i),reg)
              ++ Binop (A0,And,A0,A2)
              ++ !ls
          done;
        !ls
    end

let store a s ofs reg sp =
  if a
    then
      if s=4
        then Sw (A0,ofs,reg)
        else begin (* Value to be stored is on stack *)
          let ls = ref Nop in
          for i = 0 to s/4-1 do
            ls := Lw (A2,add ofs (mul four (of_int i)),SP)
              ++ Sw (A2,add sp (mul four (of_int i)),reg)
              ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if s > 4 (* Value to store is on stack *)
        then
          for i=0 to s-1 do
            ls := Lb (A2,add sp (of_int i),SP)
            ++ Sb (A2,add ofs (of_int i),reg)
            ++ !ls
          done
        else
          for i=0 to s-1 do
            ls := Unop (A0,Sll 4,A0)
              ++ Sb (A2,add ofs (of_int i),reg)
              ++ Binop (A0,And,A0,A2)
              ++ !ls
          done;
    end

(* convert expr to a sequence of mips instructions *)
(* arg_loc gives the offset of each argument relatively to $fp
 * Result is put on stack if its size is > 4, $a0 otherwise *)
let rec expr arg_loc sp =
  let store_args f el =
    let f = Smap.find f f_map in
    let sz = Array.to_list f.locsz in
    let rec store_args sp el sz = match el,sz with
      | [],_ -> Nop
      | e::el,(a,s)::sz ->
          (expr arg_loc sp e)
          ++(if s>4 then Nop else store a s sp SP sp)
          ++store_args (sp-s) el sz
      | _,_ -> assert false
    in
    store_args 0 el sz
  in function
  | Mconst n -> Li (A0,n)
  | Mla s -> La (A0,s)
  (*| Mload (a,s,n,Maddr i) ->
    let ofs = add n (of_int arg_loc.(i)) in
    let sp = sp - ((s+3)/4)*4 in
    load a s ofs FP sp*)
  | Mload (a,s,n,e) ->
      (expr arg_loc sp e)
      ++(Move (A1,A0))
      ++load a s n A1 (sp-s)
  | Mstor (a,s,e,n,f) ->
    (expr arg_loc sp f)
    ++(Sw (A0,0,sp))
    ++(expr arg_loc (sp-4) e)
    ++(Lw (A2,0,sp))
    ++store a s n A1
  | Maddr i -> Unop (A0,Addi (of_int arg_loc.(i)),FP)
  | Mcall (s,f,el) ->
      Unop (SP,Addi (of_int sp),SP)
      ++store_args f el
      ++Jal f
      ++Unop (Sp,Subi (of_int sp),SP)
  | Munop (u,e) -> (expr arg_loc sp e)++Unop(A0,u,A0)
  | Mbinop (o,e0,e1) ->
      expr arg_loc sp e0
      ++store true 4 sp SP sp
      ++expr arg_loc (sp-4) e1
      ++Move (A1,A0)
      ++load true 4 sp SP sp
      ++Binop (A0,o,A0,A1)
  | Mand (e1,e2) ->
      condition sp e1 (expr arg_loc sp e2 ++ Binop (A0,Sltu,ZERO,A0))
        (Binop (A0,Sltu,ZERO,A0))
  | Mor (e1,e2) ->
      condition sp e1 (Binop (A0,Sltu,ZERO,A0)
        (expr arg_loc sp e2 ++ (Binop (A0,Sltu,ZERO,A0)))

and condition arg_loc sp e b1 b2 = match e with
  | Mconst n -> if n=zero then b2 else b1
  | Mand (e1,e2) ->
      condition arg_loc sp e1 (condition arg_loc sp e2 b1 b2) b2
  | Mor (e1,e2) ->
      condition arg_loc sp e1 b1 (condition arg_loc sp e2 b1 b2)
  | Munop (Muli zero,e) ->
      expr arg_loc sp e ++ b2
  | Munop (Neg,e) ->
      condition arg_loc sp e b1 b2
  | Munop (Slti n,e) ->
      let l = fresh () in
      expr arg_loc sp e
      ++ (if n = zero
            then Ubch (Bltz,A0,l)
            else Ubch (Blti n,A0,l))
      ++b2++Label l++b1
  | Munop (Sgti n,e) ->
      let l = fresh () in
      expr arg_loc sp e
      ++ (if n = zero
            then Ubch (Bgtz,A0,l)
            else Ubch (Bgti n,A0,l))
      ++b2++Label l++b1
  | Munop (Seqi n,e) ->
      let l = fresh () in
      expr arg_loc sp e
      ++ (if n = zero
            then Ubch (Beqz,A0,l)
            else Ubch (Beqi n,A0,l))
      ++b2++Label l++b1
  | Munop (Snei n,e) ->
      let l = fresh () in
      expr arg_loc sp e
      ++ (if n = zero
            then Ubch (Bnez,A0,l)
            else Ubch (Bnei n,A0,l))
      ++b2++Label l++b1
  | Munop (Seq,e1,e2) ->
      let l = fresh () in
      expr arg_loc sp e
      ++ Bbch (Beq,e1,e2,l)
      ++ b2 ++ Label l ++ b1
