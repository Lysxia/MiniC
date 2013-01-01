open Typing

let print_tident h = Format.fprintf h "%d"

let print_tname h = Format.fprintf h "%s"

let rec string_of_tt = function
  | V -> "void"
  | I -> "int"
  | C -> "char"
  | S i -> "struct "^(string_of_int i)
  | U i -> "union "^(string_of_int i)
  | P (i,t) -> (string_of_tt t)^(String.make i '*')
  | Null -> "typenull"

let rec print_elist h = function
  | [] -> ()
  | [e] -> print_texpr h e
  | e::t -> Format.fprintf h "%a,%a" print_texpr e print_elist t

and print_texpr h {tdesc=e ; t=t} = match e with
  | TCi i -> Format.fprintf h "%s" (Int32.to_string i);
  | TCs s -> Format.fprintf h "\"%s\"" s
  | TLoc i -> print_tident h i
  | TGlo n -> print_tname h n
  | TDot (e,i) ->
    Format.fprintf h "@[(@[%a@].%a)@]" print_texpr e print_tident i
  | TAssign (e1,e2) ->
    Format.fprintf h "@[(%a=%a)@]@," print_texpr e1 print_texpr e2
  | TCall (f,arg) ->
    Format.fprintf h "@[%a(%a)@]" print_tname f print_elist arg
  | TUnop (_,e) ->
    Format.fprintf h "@[#(%a)@]" print_texpr e
  | TBinop (_,e1,e2) ->
    Format.fprintf h "@[(%a$%a)@]@," print_texpr e1 print_texpr e2
  | TSizeof t ->
    Format.fprintf h "sizeof(%s)" (string_of_tt t)

let rec print_tinstr h = function
  | TNop -> Format.fprintf h "Nop;@\n"
  | TExpr e ->
    Format.fprintf h "@[%a;@]\t%s@\n" print_texpr e (string_of_tt e.t)
  | TIf (e,i1,i2) ->
    Format.fprintf h
      "if (@[%a@])@\n  @[%a@]@\nelse@\n  @[%a@]@\n"
      print_texpr e print_tinstr i1 print_tinstr i2
  | TWhile (e,i) ->
    Format.fprintf h "while (@[%a@])@\n@[<2>%a@]@\n"
      print_texpr e print_tinstr i
  | TFor (ini,e,inc,i) ->
    Format.fprintf h
      "for (@[%a@] ; @[%a@] ; @[%a@])@\n@[<hov 2>%a@]@\n"
      print_elist ini print_texpr e print_elist inc print_tinstr i
  | TBloc il ->
    List.iter (print_tinstr h) il
  | TReturn None -> Format.fprintf h "return;@\n"
  | TReturn (Some e) -> Format.fprintf h "return @[%a@]"
    print_tinstr (TExpr e)

let print_arg h n l =
  if n>0
    then begin
      Format.fprintf h "%s 0" (string_of_tt l.(0));
      for i = 1 to n-1 do
        Format.fprintf h ",%s %i" (string_of_tt l.(i)) i
      done
    end

let print_locals h n l =
  for i = n to Array.length l -1 do
    Format.fprintf h "%s %d;@\n" (string_of_tt l.(i)) i
  done

let print_tfct h {tret=t;tfid=f;formals=n;locals=l;tbody=il} =
  Format.fprintf h "%s %a(%a)@\n  @[%a@\n%a@]@\n"
    (string_of_tt t) print_tname f (fun h -> print_arg h n) l
    (fun h -> print_locals h n) l
    (fun h -> List.iter (print_tinstr h)) il

let print_tfile h (_,fl,_) =
  List.iter (print_tfct h) fl
