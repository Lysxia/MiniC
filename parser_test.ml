open Ast
open Format

let rec typestring = function
  | Struct s -> "struct "^s
  | Union s -> "union "^s
  | Point (n,t) ->
      (typestring t)^(String.make n '*')
  | Int -> "int"
  | Char -> "char"
  | Void -> "void"

let rec print_separ_list f ch = function
  | [] -> ()
  | [h] -> f ch h
  | h::t -> f ch h; fprintf ch ", ";
      print_separ_list f ch t

let rec print_expr h e =
  print_edesc h e.desc

and print_edesc h = function
  | Cint i -> fprintf h "%s" (Int32.to_string i)
  | Cstring s -> fprintf h "\"%s\"" s
  | Ident s -> fprintf h "%s" s
  | Dot (e,s) -> fprintf h "%a.%s"
      print_expr e s
  | Assign (e1,e2) -> fprintf h "(%a = %a)"
      print_expr e1 print_expr e2
  | Call (f,a) -> fprintf h "%s(%a)" f
      (print_separ_list print_expr) a
  | Unop (o,e) -> fprintf h "%s(%a)" 
      (match o with
         | Star -> "*" | Incrp -> "++"
         | Incrs -> "+s" | Decrp -> "--"
         | Decrs -> "-s" | Address -> "&"
         | Not -> "!" | Uminus -> "-" | Uplus -> "+")
      print_expr e
  | Binop (o,e1,e2) ->
      fprintf h "(%a%s%a)" print_expr e1
        (match o with
           | Or -> "||" | And -> "&&" | Eq | Neq -> "=?"
           | Lt | Leq | Gt | Geq -> "<>"
           | Add | Sub -> "+" | Mul | Div | Mod -> "%" )
        print_expr e2
  | Sizeof t -> fprintf h "sizeof(%s)" (typestring t)

let print_vdec h { desc=t,id ; loc=_ } =
  fprintf h "%s %s" (typestring t) id

let rec print_i h = function
  | Instr i -> fprintf h "%a;@," print_idesc i.desc
  | Expr e -> fprintf h "%a;@," print_expr e

and print_idesc h =
  function
  | Nop -> ()
  | If (e,i1,i2) -> fprintf h "if (%a)@[<hv>@,%a@,else %a@]"
      print_expr e print_i i1 print_i i2
  | While (e,i) -> fprintf h "while (%a)@[<hv>@,%a@]"
      print_expr e print_i i
  | For (e1,e2,e3,i) -> fprintf h "for (%a ; %a ; %a)@[<hv>@,%a@]"
      (print_separ_list print_expr) e1
      (fun h -> function None -> () | Some e -> print_expr h e) e2
      (print_separ_list print_expr) e3 print_i i
  | Bloc (v,i) -> fprintf h "{@,%a%a}"
      (fun h -> List.iter (fun v -> fprintf h "%a;@ " print_vdec v;)) v
      (fun h -> List.iter (print_i h)) i
  | Return (Some e) -> fprintf h "return %a"
      print_expr e
  | Return None -> fprintf h "return"


let rec print_dec h = function
  | V v -> fprintf h "%a;@\n" print_vdec v
  | Dec d -> print_ddesc h d.desc; force_newline ()

and print_ddesc h = function
  | Typ (t,vl) -> fprintf h "%s {@[@ %a@]}"
      (typestring t)
      (fun h -> List.iter (fun v -> fprintf h "%a;@ " print_vdec v;)) vl
  | Fct (t,f,arg,v,i) -> fprintf h "%s %s(%a) @[<hv>%a@]"
      (typestring t) f
      (print_separ_list print_vdec) arg
      print_idesc (Bloc (v,i))

let print_ast h =
  List.iter (print_dec h)

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let h = open_in Sys.argv.(i) in
    let buf = Lexing.from_channel h in
    begin
      try
        Format.printf "File %s\n@?" Sys.argv.(i);
        print_ast std_formatter (Parser.prog Lexer.token buf);
      with
        | Error.E (sp,ep,s) -> Error.prerr Sys.argv.(i) sp ep s
        | Parser.Error -> Error.catch Sys.argv.(i) buf
    end;
    close_in h;
    print_newline ()
  done
