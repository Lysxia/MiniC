open Iselect
open Int32
open Format

let rec unop_string = function
  | Neg -> "neg"
  | Andi n -> sprintf "andi(%s)" (to_string n)
  | Addi n -> sprintf "addi(%s)" (to_string n)
  | Subi n -> sprintf "subi(%s)" (to_string n)
  | Muli n -> sprintf "muli(%s)" (to_string n)
  | Divi n -> sprintf "div_i(%s)" (to_string n)
  | Remi n -> sprintf "rem_i(%s)" (to_string n)
  | Slti n -> sprintf "slti(%s)" (to_string n)
  | Sltiu n -> sprintf "sltiu(%s)" (to_string n)
  | Sgti n -> sprintf "sgti(%s)" (to_string n)
  | Seqi n -> sprintf "seqi(%s)" (to_string n)
  | Snei n -> sprintf "snei(%s)" (to_string n)
  | Srl k -> sprintf "srl(%d)" k
  | Sll k -> sprintf "sll(%d)" k

let rec binop_string = function
  | Add -> "add"
  | Div -> "div"
  | Mul -> "mul"
  | Sub -> "sub"
  | Rem -> "rem"
  | Slt -> "slt"
  | Sle -> "sle"
  | Seq -> "seq"
  | Sne -> "Sne"
  | Sltu -> "sltu"
  | Or -> "or"


let rec print_expr h = function
  | Mconst n -> fprintf h "%s" (to_string n)
  | Munop (u,e) ->
      fprintf h "(%s %a)"
        (unop_string u) print_expr e
  | Mbinop (o,e1,e2) ->
      fprintf h "(%s %a %a)"
        (binop_string o)
        print_expr e1
        print_expr e2
  | Maddr i ->
      fprintf h "&%d" i
  | Mla s ->
      fprintf h "%s" s
  | Mload (_,sz,ofs,addr) ->
      fprintf h "(LOAD_%d %s %a)"
        sz (to_string ofs)
        print_expr addr
  | Mstor (_,sz,e,ofs,addr) ->
      fprintf h "(STOR_%d %a %s %a)"
        sz
        print_expr e
        (to_string ofs)
        print_expr addr
  | Mand (e1,e2) ->
      fprintf h "(%a && %a)"
        print_expr e1
        print_expr e2
  | Mor (e1,e2) ->
      fprintf h "(%a || %a)"
        print_expr e1
        print_expr e2
  | Mcall (sz,f,l) ->
      fprintf h "%s:%d(%a)"
        f sz print_elist l

and print_elist h = function
  | [] -> ()
  | [e] -> print_expr h e
  | e::t ->
      print_expr h e;
      fprintf h ",";
      print_elist h t

let rec print_instr h = function
  | Nop -> fprintf h "Nop@\n"
  | Expr e ->
      fprintf h "%a;@\n"
        print_expr e
  | If (e,i1,i2) ->
      fprintf h "if (%a)@\n  @[%a@]@\nelse@\n  @[%a@]@\n"
        print_expr e
        print_instr i1
        print_instr i2
  | While (e,i) ->
      fprintf h "while (%a)@\n  @[%a@]@\n"
        print_expr e
        print_instr i
  | For (init,cond,inc,i) ->
      fprintf h "for (%a;%a;%a)@\n  @[%a@]@\n"
        print_elist init
        print_expr cond
        print_elist inc
        print_instr i
  | Bloc il ->
      List.iter (print_instr h) il
  | Ret None -> fprintf h "return;@\n"
  | Ret (Some e) ->
      fprintf h "return %a;@\n"
        print_expr e

let print_comma_sep_array h n a =
  for i = 0 to n-1 do
    fprintf h "%d" (snd a.(i));
    if i<n-1 then
      fprintf h ","
  done

let print_fct h
  {
    retsz=rs;
    fid=f;
    locsz=a;
    formals=fml;
    locals=lcl;
    body=i;
  } =
  fprintf h "%d %s(%a)@\n  @[%a@]@\n"
    rs f (fun h -> print_comma_sep_array h fml) a
    print_instr i

let print_file h f =
  List.iter (print_fct h) f
