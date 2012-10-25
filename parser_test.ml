open Ast

let d = ref 0

let margin () =
  print_string (String.make (!d*4) ' ')

let print_type = function
  | Struct s -> print_string "S s"
  | Union s -> print_string "U s"
  | Point _ -> print_string "'t*"
  | _ -> print_string "'t"

let rec print_expr e =
  print_edesc e.desc

and print_edesc = function
  | Cint i -> Printf.printf "%d" i
  | Cstring s | Ident s -> Printf.printf "%s" s
  | Dot (e,s) -> print_string "(";
     print_expr e;
     print_string ")";
     Printf.printf ".%s" s
  | Assign (e1,e2) -> print_string "(";
     print_expr e1;
     print_string ")=";
     print_expr e2;
  | Call (f,a) -> print_string (f^"(");
      List.iter (fun e -> print_expr e; print_string ",") a;
      print_string (")"^(if a=[] then "" else "\b"))
  | Unop (_,e) -> print_string "UNOP(";
      print_expr e;
      print_string ")";
  | Binop (_,e1,e2) -> print_string "(";
      print_expr e1;
      print_string ")+(";
      print_expr e2;
  | Sizeof _ -> print_string "sizeof()"

let print_vs v =
  let name (_,id) = id in
  print_string ("var "^(name v.desc))

let rec print_i = function
  | Instr i -> margin (); print_idesc i.desc
  | Expr e -> margin (); print_expr e; print_newline ()

and print_idesc = function
  | Nop -> ();
  | If (e,i1,i2) -> print_string "if (";
    print_expr e;
    print_string ")\n";
    incr d;
    print_i i1;
    decr d;
    print_string "\n"; margin ();
    print_string "else\n";
    incr d;
    print_i i2;
    decr d;
  | While (e,i) -> print_string "while (";
    print_expr e;
    print_string ")\n";
    incr d;
    print_i i;
    decr d;
  | Bloc (v,i) ->
    print_string "\b\b\b\b{\n";
    List.iter (fun v -> margin(); print_vs v; print_newline()) v;
    List.iter print_i i;
    print_string "\b\b\b\b}\n";
  | Return e ->
    print_string "Return ";
    (match e with None -> ()
               | Some e -> print_expr e);
    print_newline ()

let rec print_stmt = function
  | V v -> print_vs v; print_newline ();
  | Stmt d -> print_sdesc d.desc; print_newline ();

and print_sdesc = function
  | Typ (t,_) | Typ (t,_) -> print_string "Constructor"; print_type t
  | Fct (_,f,arg,v,i) -> print_string (f^"(");
    List.iter (fun v -> print_vs v; print_string ",") arg;
    print_string (if arg=[] then ")\n" else "\b)\n");
    incr d;
    List.iter (fun v -> margin (); print_vs v; print_newline()) v;
    List.iter print_i i

let print_ast =
  List.iter print_stmt

let () =
  for i = 1 to Array.length Sys.argv do
    Printf.printf "File %s\n" Sys.argv.(i);
    let h = open_in Sys.argv.(i) in
    print_ast (Parser.prog Lexer.token (Lexing.from_channel h));
    print_newline ()
  done
