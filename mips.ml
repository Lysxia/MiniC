(** Mini-C Compiler **)
(* Li-yao Xia *)

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
  | Comment of string
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

let rec last = function
  | Concat (t1,t2) ->
      begin try
        last t2
      with Not_found -> last t1
      end
  | Comment _ | Nop -> raise Not_found
  | t -> t

let last2 t =
  let rec last2 = function
    | Concat (t1,t2) ->
        let i = last2 t2 in
        if List.length i = 2 then i
        else if List.length i = 0 then last2 t1
        else (match last2 t1 with
          | [] -> i
          | [i_] | [_;i_] -> i_::i
          | _ -> assert false)
    | Comment _ | Nop -> []
    | t -> [t]
  in last2 t

let (++) c1 c2 = match c1,c2 with
  | Nop,_ -> c2
  | _,Nop -> c1
  | _,_ -> Concat (c1,c2)

let free = ref 0
let fresh () = incr free; string_of_int !free

let free2 = ref 0
let fresh2 () = incr free2; string_of_int !free2

let f_map:fct Smap.t ref = ref Smap.empty

let recmain = ref false

let load a s ofs reg sp =
  if a
    then
      if s=4
        then Lw (A0,ofs,reg)
        else begin
          let sp = (sp-s+1)/4*4 in
          let ls = ref Nop in
          for i = 0 to s/4-1 do
            ls := Lw (A2,ofs+4*i,reg)
              ++ Sw (A2,sp+4*i,SP)
              ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if s > 4
        then
          let sp = sp-s+4 in
          for i=0 to s-1 do
            ls := Lb (A2,ofs+i,reg)
              ++ Sb (A2,sp+i,SP)
              ++ !ls
          done
        else begin
          assert (s>0);
          for i=0 to s-2 do
            ls :=
              Unop (A0,Sll 4,A0)
              ++ Lb (A2,ofs+i,reg)
              ++ Binop (A0,Or,A0,A2)
              ++ !ls
          done;
          ls := Lb (A0,ofs+s-1,reg) ++ !ls;
        end;
        !ls
    end

(* Store last calculated value *)
let store a s ofs reg sp =
  if a
    then
      if s=4
        then Sw (A0,ofs,reg)
        else begin
          let sp = (sp-s+1)/4*4 in
          let ls = ref Nop in
          for i = 0 to s/4-1 do
            ls := Lw (A2,sp+4*i,SP)
              ++ Sw (A2,ofs+4*i,reg)
              ++ !ls
          done;
          !ls
        end
    else begin
      let ls = ref Nop in
      if s>4
        then begin
          let sp = sp-s+4 in
          for i=0 to s-1 do
            ls := Lb (A2,sp+i,SP)
            ++ Sb (A2,ofs+i,reg)
            ++ !ls
          done;
        end
        else begin
          for i=s-1 downto 1 do
            ls :=
              Unop (A0,Srl 4,A0)
              ++ Sb (A0,ofs+i,reg)
              ++ !ls
          done;
          ls := Sb (A0,ofs,reg) ++ !ls;
        end;
        !ls
    end

(* brch is an instruction which branches to l1
 * which corresponds to b1 *)
let branching brch_instr l1 l2 b1 b2 =
  brch_instr
  ++ b2
  ++ J l2
  ++ Label l1
  ++ b1
  ++ Label l2

(* e1 and e2 are int or char typed expressions*)
(* stores the outputs in A0 and A1 *)
let rec word_pair argpos sp e1 e2 =
  let sp = (sp-3)/4*4 in
  expr argpos sp e2
  ++ Sw (A0,sp,SP)
  ++ expr argpos (sp-4) e1
  ++ Lw (A1,sp,SP)

(* convert expr to a sequence of mips instructions *)
and expr argpos sp =
  let store_args f el =
    let rec store_args sp el pos al sz = match el,pos,al,sz with
      | [],_,_,_ -> Nop
      | e::el,p::pos,a::al,s::sz ->
          expr argpos sp e
          ++ (if s>4 then Nop else store a s sp SP sp)
          ++ store_args (sp-s) el pos al sz
      | _,_,_,_ -> assert false
    in
    store_args 0 el f.argpos f.argal f.argsz
  in function
  | Mconst n -> Li (A0,n)
  | Mla s -> La (A0,s)
  | Mload (a,s,n,Maddr i) ->
      let ofs = (to_int n)+argpos.(i) in
      Comment (string_of_int argpos.(i)) ++
      load a s ofs FP sp
  | Mload (a,sz,n,Mcall_addr (s,f,el)) ->
      let f = Smap.find f !f_map in
      let sp = (sp-3)/4*4 in
      let hd,ft =
        if sp = 0
          then Nop,Nop
          else Unop (SP,Addi (of_int sp),SP),
               Unop (SP,Subi (of_int sp),SP) in
      let load_field =
        if s > 4
          then load a sz (to_int n-s+4) SP sp
          else Unop (A0,Srl (s+to_int n),A0) in
      hd
      ++ store_args f el
      ++ Jal f.ident
      ++ ft
      ++ load_field
  | Mload (a,s,n,e) ->
      expr argpos sp e
      ++ Move (A1,A0)
      ++ load a s (to_int n) A1 sp
  | Mstor (a,s,e,n,Maddr i) ->
      let ofs = (to_int n)+argpos.(i) in
      expr argpos sp e
      ++ store a s ofs FP sp
  | Mstor (a,s,e,n,f) ->
      (* We first compute the address, which must be aligned *)
      let sp = (sp-3)/4*4 in
      expr argpos sp f
      ++ Sw (A0,sp,SP)
      ++ expr argpos (sp-4) e
      ++ Lw (A1,sp,SP)
      ++ store a s (to_int n) A1 (sp-4)
  | Maddr i -> Unop (A0,Addi (of_int argpos.(i)),FP)
  | Mcall (_,"putchar",el) ->
      expr argpos sp (List.hd el)
      ++ Li (V0,of_int 11)
      ++ Syscall
  | Mcall (_,"sbrk",el) ->
      expr argpos sp (List.hd el)
      ++ Li (V0,of_int 9)
      ++ Syscall
      ++ Move (A0,V0)
  | Mcall (s,f,el) ->
      if f="main"
        then recmain := true;
      let f = Smap.find f !f_map in
      let sp = (sp-3)/4*4 in
      let hd,ft =
        if sp = 0
          then Nop,Nop
          else Unop (SP,Addi (of_int sp),SP),
               Unop (SP,Subi (of_int sp),SP) in
      hd
      ++ store_args f el
      ++ Jal f.ident
      ++ ft
  | Munop (u,e) -> (expr argpos sp e)++Unop(A0,u,A0)
  | Mbinop (o,e1,e2) ->
      word_pair argpos sp e1 e2
      ++ Binop (A0,o,A0,A1)
  | Mand (e1,e2) ->
      condition argpos sp e1
        (expr argpos sp e2 ++ Binop (A0,Sltu,ZERO,A0))
        (Li (A0,zero))
  | Mor (e1,e2) ->
      condition argpos sp e1 (Li (A0,one))
        (expr argpos sp e2 ++ (Binop (A0,Sltu,ZERO,A0)))
  | Mcall_addr _ -> assert false

and condition arg sp e b1 b2 = match e with
  | Mconst n -> if n=zero then b2 else b1
  | Mand (e1,e2) -> condition arg sp e1 (condition arg sp e2 b1 b2) b2
  | Mor (e1,e2) -> condition arg sp e1 b1 (condition arg sp e2 b1 b2)
  | Munop (Neg,e) -> condition arg sp e b1 b2
  | Munop (Muli zero,e) -> expr arg sp e ++ b2
  | e ->
      let l1 = "b"^fresh () in
      let l2 = l1^"_0" in
      branching (branch arg sp e l1) l1 l2 b1 b2

and branch argpos sp e l = match e with
  | Mconst n -> if n=zero then Nop else J l
  | Mand (e1,e2) ->
      let l0 = "and_fail"^fresh() in
      expr argpos sp e1
      ++ Bbch (Beq,A0,ZERO,l0)
      ++ expr argpos sp e2
      ++ Bbch(Bne,A0,ZERO,l)
      ++ Label l0
  | Mor (e1,e2) ->
      expr argpos sp e1
      ++ Bbch (Bne,A0,ZERO,l)
      ++ expr argpos sp e2
      ++ Bbch (Bne,A0,ZERO,l)
  | Munop (Neg,e) ->
      branch argpos sp e l
  | Munop (Muli zero,e) ->
      expr argpos sp e ++ J l
  | Munop (Slti n,e) ->
      expr argpos sp e
      ++ (if n = zero
            then Ubch (Bltz,A0,l)
            else Ubch (Blti n,A0,l))
  | Munop (Sgti n,e) ->
      expr argpos sp e
      ++ (if n = zero
            then Ubch (Bgtz,A0,l)
            else Ubch (Bgti n,A0,l))
  | Munop (Seqi n,e) ->
      expr argpos sp e
      ++ (if n = zero
            then Ubch (Beqz,A0,l)
            else Ubch (Beqi n,A0,l))
  | Munop (Snei n,e) ->
      expr argpos sp e
      ++ (if n = zero
            then Ubch (Bnez,A0,l)
            else Ubch (Bnei n,A0,l))
  | Mbinop (Seq,e1,e2) ->
      word_pair argpos sp e1 e2
      ++ Bbch (Beq,A0,A1,l)
  | Mbinop (Sne,e1,e2) ->
      word_pair argpos sp e1 e2
      ++ Bbch (Bne,A0,A1,l)
  | Mbinop (Slt,e1,e2) ->
      word_pair argpos sp e1 e2
      ++ Bbch (Blt,A0,A1,l)
  | Mbinop (Sle,e1,e2) ->
      word_pair argpos sp e1 e2
      ++ Bbch (Ble,A0,A1,l)
  | e ->
      expr argpos sp e
      ++ Ubch (Bnez,A0,l)

let rec instr arg ((set_return,exit) as quit) = function
  | Iselect.Nop -> Nop
  | Expr e -> expr arg 0 e
  | If (e,i1,i2) ->
      let k = fresh2 () in
      Comment ("if "^k)
      ++ condition arg 0 e
           (instr arg quit i1) (instr arg quit i2)
      ++ Comment ("endif "^k)
  | While (e,i) ->
      let l = "while"^fresh () in
      let l_ = l^"_0" in
      Comment l
      ++ J l_
      ++ Label l
      ++ instr arg quit i
      ++ Label l_
      ++ branch arg 0 e l
      ++ Comment ("end"^l)
  | For (init,cond,inc,i) ->
      let l = "for"^fresh () in
      let l_ = l^"_0" in
      Comment l
      ++ List.fold_left (++) Nop (List.map (expr arg 0) init)
      ++ J l_
      ++ Label l
      ++ instr arg quit i
      ++ List.fold_left (++) Nop (List.map (expr arg 0) inc)
      ++ Label l_
      ++ branch arg 0 cond l
      ++ Comment ("end"^l)
  | Bloc i ->
      let k = fresh2 () in
      Comment ("bloc "^k)
      ++ List.fold_left (++) Nop (List.map (instr arg quit) i)
      ++ Comment ("endbloc "^k)
  | Ret None -> Comment ("return "^fresh ()) ++ exit
  | Ret (Some e) ->
      Comment ("returnv "^fresh ())
      ++ expr arg 0 e
      ++ set_return
      ++ exit

let mk_frame sz =
  let n = Array.length sz in
  let argpos = Array.make n 0 in
  let argsz = Array.make n 0 in
  let argal = Array.make n false in
  for i = 0 to n-1 do
    let al,sz = sz.(i) in
    argsz.(i) <- sz;
    argal.(i) <- al;
    if i = 0
      then argpos.(i) <- 4-sz
      else begin
        argpos.(i) <-
          if al
            then (argpos.(i-1)-sz-3)/4*4
            else argpos.(i-1)-sz
      end
  done;
  argpos,argsz,argal

let fct
  {
    retsz=rsz;
    retal=ral;
    fid=f_;
    formals=argc;
    locals=n;
    locsz=sz;
    Iselect.body=i;
  } =
  let argpos,argsz,argal = mk_frame sz in
  let f =
    {
      ident="_"^f_;
      argpos=Array.to_list argpos;
      argsz=Array.to_list argsz;
      argal=Array.to_list argal;
      rsz=rsz;
      ral=ral;
      body=Nop;
    } in
  f_map := Smap.add f_ f !f_map;
  let frame_ofs =
    if n=0 then -8
    else (argpos.(n-1)-15)/4*4 in (* two spots for $ra and $fp *)
  (* Putting output value in the right place *)
  let set_return =
    if rsz > 4
      then store ral rsz (-rsz+4) FP 0
      else Nop
  in
  let exit =
    Move (SP,FP)
    ++ Lw (FP,frame_ofs+4,SP)
    ++ Lw (RA,frame_ofs+8,SP)
    ++ Jr RA
  in
  let alloc_frame =
    Sw (FP,frame_ofs+4,SP)
    ++ Sw (RA,frame_ofs+8,SP)
    ++ Unop (SP,Addi (of_int frame_ofs),SP)
    ++ Unop (FP,Subi (of_int frame_ofs),SP)
  in
  let body = instr argpos (set_return,exit) i in
  f.body <-
    Label f.ident
    ++ alloc_frame
    ++ body
    ++ (if (try last body = Jr RA with Not_found -> false)
          then Nop
          else exit)

let collect acc defrost f =
  if f.fid = "main"
    then begin
      (* Abort compiling main *)
      defrost := (fun () -> fct f);
      let argpos,_,_ = mk_frame f.locsz in
      (* Insert a dummy "_main" element *)
      let m =
        {
          ident="_"^f.fid;
          argpos=[0;-4];
          argsz=[4;4];
          argal=[true;true];
          rsz=4;
          ral=true;
          body=Nop;
        } in
      f_map := Smap.add f.fid m !f_map;
      let start =
        if f.formals=0
          then Nop
          else Sw (A0,0,SP) ++ Sw (A1,-4,SP) in
      let frame_ofs =
        if f.locals=0
          then 0
          else (argpos.(f.locals-1)-4)/4*4 in
      let exit =
        Li (V0,of_int 10)
        ++ Syscall
      in
      let alloc_frame =
        Move (FP,SP)
        ++ (if frame_ofs = 0
              then Nop
              else Unop (SP,Addi (of_int frame_ofs),SP))
      in
      let body = instr argpos (Nop,exit) f.Iselect.body in
      acc := Some (
        Label "main"
        ++ start
        ++ alloc_frame
        ++ body
        ++ (if last2 body = [Li (V0,of_int 10); Syscall]
              then Nop
              else exit))
    end
  else fct f

(**)
let reset () =
  Iselect.reset ();
  free := 0;
  f_map := Smap.empty;
  recmain := false
