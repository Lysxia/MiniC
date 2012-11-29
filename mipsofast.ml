type reg = string

type mips =
  | Li    of reg*Int32.t
  | La    of reg*string
  | RR    of string*reg*reg (* Instructions sorted by argument types *)
  | RR32  of string*reg*reg*Int32.t
  | RRR   of string*reg*reg*reg
  | Mem   of string*reg*int*reg (* lw,sw etc Offset on 16 signed bits *)
  | Label of string
  | Cmmnt of string

type mips2 =
  | Align of int*string
  | Ascii of string
  | Byte of string
  | Space of int
  | Word of Int32.t

type code = { text:mips list ; data:mips2 list }

let legal_reg = ref
  [ "t8";"t9";"a0";"a1";"a3";"v0";"v1";"sp";"fp";"ra";"gp" ]

let () =
  for i = 0 to 7 do
    legal_reg := ("t"^(string_of_int i))::("s"^(string_of_int i))::!legal_reg
  done

let legal_reg r =
  let legal_reg =
    List.fold_left (fun a x-> Sset.add x a) Sset.empty !legal_reg
  in Sset.mem r legal_reg

let print_instr h = function
  | Li (r,i) -> assert (legal_reg r);
      Printf.fprintf h "li\t$%s, %s\n" r (Int32.to_string i)
  | La (r,s) -> assert (legal_reg r);
      Printf.fprintf h "la\t$%s, %s\n" r s
  | RR (s,r1,r2) -> assert (legal_reg r1 && legal_reg r2);
      Printf.fprintf h "%s\t$%s, $%s\n" s r1 r2
  | RRR (s,a,b,c) -> assert (legal_reg a && legal_reg b && legal_reg c);
      Printf.fprintf h "%s\t$%s, $%s, $%s\n" s a b c
  | RR32 (s,a,b,c) -> assert (legal_reg a && legal_reg b);
      Printf.fprintf h "%s\t$%s, $%s, %s\n" s a b (Int32.to_string c)
  | Label s -> Printf.fprintf h "_%s:\n" s
  | Mem (s,a,b,c) -> assert (legal_reg a && legal_reg c);
      Printf.fprintf h "%s\t$%s, %s, $%s\n" s a (string_of_int b) c
  | Cmmnt s -> Printf.fprintf h "//%s\n" s
