open Iselect

let rec print_unop h = function
  | Neg -> Format.fprintf h "neg"
  | Addi n ->
      Format.fprintf h "addi %s" (Int32.to_string n)
  | Subi n ->
      Format.fprintf h "subi %s" (Int32.to_string n)
  | Muli n ->
      Format.fprintf h "muli %s" (Int32.to_string n)
  | Divi n ->
      Format.fprintf h "div(i) %s" (Int32.to_string n)
  | Remi n ->
      Format.fprintf h "rem(i) %s" (Int32.to_string n)
  | Slti n ->
      Format.fprintf h "slti %s" (Int32.to_string n)
  | Sltiu n ->
      Format.fprintf h "sltiu %s" (Int32.to_string n)
  | Sll k ->
      Format.fprintf h "sll %d" k

let rec string_binop = function
  | Add -> "add"
  | Div -> "div"
  | Mul -> "mul"
  | Sub -> "sub"
  | Rem -> "rem"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sltu -> "sltu"


let rec print_expr h = function
  | Mconst n -> Format.fprintf h "%s" (Int32.to_string n)
  | Mmove (e1,e2) ->
      Format.fprintf h "(%a=%a)"
        print_expr e1 print_expr e2
  | Munop (u,e) ->
      Format.fprintf h "(%a %a)"
        print_unop u print_expr e
  | Mbinop (o,e1,e2) ->
      Format.fprintf h "(%s %a %a)"
        (string_binop o)
        print_expr e1
        print_expr e2
  | Mloc i ->
      Format.fprintf h "x%d" i
  | Maddr i ->
      Format.fprintf h "&%d" i
  | Mla s ->
      Format.fprintf h "%s" s
  | Mload (sz,ofs,addr) ->
      Format.fprintf h "(LOAD_%d %s %a)"
        sz (Int32.to_string ofs)
        print_expr addr
  | Mlb (ofs,addr) ->
      Format.fprintf h "(lb %s %a)"
        (Int32.to_string ofs)
        print_expr addr
  | Mlw (ofs,addr) ->
      Format.fprintf h "(lw %s %a)"
        (Int32.to_string ofs)
        print_expr addr
  | Mstor (sz,e,ofs,addr) ->
      Format.fprintf h "(STOR_%d %a %s %a)"
        sz
        print_expr e
        (Int32.to_string ofs)
        print_expr addr
  | Msb (e,ofs,addr) ->
      Format.fprintf h "(sb %a %s %a)"
        print_expr e
        (Int32.to_string ofs)
        print_expr addr
  | Msw (e,ofs,addr) ->
      Format.fprintf h "(sw %a %s %a)"
        print_expr e
        (Int32.to_string ofs)
        print_expr addr
  | Mand (e1,e2) ->
      Format.fprintf h "(%a && %a)"
        print_expr e1
        print_expr e2
  | Mor (e1,e2) ->
      Format.fprintf h "(%a || %a)"
        print_expr e1
        print_expr e2
  | Mcall (f,l) ->
      Format.fprintf h "%s(%a)"
        f
        print_elist l

and print_elist h = function
  | [] -> ()
  | [e] -> print_expr h e
  | e::t ->
      print_expr h e;
      Format.fprintf h ",";
      print_elist h t

let rec print_instr h = function
  | Nop -> Format.fprintf h "Nop@\n"
  | Expr e ->
      Format.fprintf h "%a;@\n"
        print_expr e
  | If (e,i1,i2) ->
      Format.fprintf h "if (%a)@\n  @[%a@]@\nelse@\n  @[%a@]@\n"
        print_expr e
        print_instr i1
        print_instr i2
  | While (e,i) ->
      Format.fprintf h "while (%a)@\n  @[%a@]@\n"
        print_expr e
        print_instr i
  | For (init,cond,inc,i) ->
      Format.fprintf h "for (%a;%a;%a)@\n  @[%a@]@\n"
        print_elist init
        print_expr cond
        print_elist inc
        print_instr i
  | Bloc il ->
      List.iter (print_instr h) il
  | Ret None -> Format.fprintf h "return;@\n"
  | Ret (Some e) ->
      Format.fprintf h "return %a;@\n"
        print_expr e

let print_comma_sep_array h a =
  let n = Array.length a in
  for i = 0 to n-1 do
    Format.fprintf h "%d" a.(i);
    if i<n-1 then
      Format.fprintf h ","
  done

let print_fct h {fid=f;argsz=a;body=i;retsz=r} =
  Format.fprintf h "%d %s(%a)@\n  @[%a@]@\n"
    r f print_comma_sep_array a
    (fun h -> List.iter (print_instr h)) i

let print_file h (f,v,dat) =
  List.iter (print_fct h) f
