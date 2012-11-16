(** Mini-C Lexer **)
(** Li-yao Xia **)

{
  open Lexing
  open Parser

  let raise_err lexbuf s =
    raise (Error.E (lexeme_start_p lexbuf,lexeme_end_p lexbuf,s))

  let kwd_tbl =
    ["char", 	CHAR;
     "else", 	ELSE;
     "for", 	FOR;
     "if", 	IF;
     "int", 	INT;
     "return", 	RETURN;
     "sizeof", 	SIZEOF;
     "struct", 	STRUCT;
     "union", 	UNION;
     "void", 	VOID;
     "while", 	WHILE]

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      try List.assoc s kwd_tbl with _ -> IDENT s

  let char_of_escape lexbuf = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | '\\' | '\'' as s -> s
    | c -> raise_err lexbuf ("Unknown escape sequence \'"^
        (String.make 1 c)^"\'")
}

let space = [' ' '\t']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let octal = ['0'-'7']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']

let ident = (letter|'_') (letter|digit|'_')*

let integer = digit* | ("0x" hexa+)

let car = (['\032'-'\127']#['\\' '\'' '"'])
let escape = "\\" | "n" | "\""

rule token = parse
  | '\n'	{ new_line lexbuf; token lexbuf }
  | space+	{ token lexbuf }
  | ident as id	{ id_or_kwd id }
  | '*' 	{ STAR }
  | '(' 	{ LPAR }
  | ')' 	{ RPAR }
  | '[' 	{ LBKT }
  | ']' 	{ RBKT }
  | '{' 	{ LBRC }
  | '}' 	{ RBRC }
  | ';' 	{ SEMICOLON }
  | ',' 	{ COMMA }
  | '.' 	{ DOT }
  | "->"	{ ARROW }
  | '=' 	{ ASSIGN }
  | "++"	{ INCR }
  | "--" 	{ DECR }
  | '&' 	{ ADDRESS }
  | '!' 	{ NOT }
  | '+' 	{ PLUS }
  | '-' 	{ MINUS }
  | "=="	{ EQ }
  | "!="	{ NEQ }
  | '<' 	{ LT }
  | "<="	{ LEQ }
  | '>' 	{ GT }
  | ">="	{ GEQ }
  | '/' 	{ DIV }
  | '%' 	{ MOD }
  | "&&"	{ AND }
  | "||"	{ OR }
  | eof 	{ EOF }
  (* signed 32 bits integer in C vs signed 31 bit integer in OCaml...  *)
  | "0" octal+ as n 	{ CST (int_of_string ("0o"^n)) }
  | integer as n 	{ CST (int_of_string n) }
  | (digit|letter|'_')+	{
      raise_err lexbuf "Not a valid identifier or number" }
  | "\"" 				{ STR (str lexbuf) }
  | "\'" (car|'"' as c) "\'"		{ CST (int_of_char c) }
  | "\'\\" (car as c) "\'"		{
      CST (int_of_char (char_of_escape lexbuf c)) }
  | "\'" ("\\x" hexa hexa as c) "\'"	{
      c.[0] <- '0'; CST (int_of_string c) }
  | "\'\\x" _ _ 	 		{
      raise_err lexbuf "\\x used with no following hex digits"}
  | "/*"	{ comment lexbuf }
  | "//" [^ '\n' ]* 	{ token lexbuf }
  | "//" [^ '\n' ]* eof	{ EOF }
  | _ as c		{
      raise_err lexbuf ("Illegal character: "^String.make 1 c) }

and str = parse
  | "\\"	{ escape lexbuf }
  | car as c	{ (String.make 1 c)^str lexbuf }
  | "\""	{ "" }
  | _		{ raise_err lexbuf "Missing terminating \" character" }

and escape = parse
  | _ as c	{
      (String.make 1 (char_of_escape lexbuf c))^str lexbuf }
  | '"' 	{ "\""^str lexbuf }
  | "x" (hexa hexa as n)
      { (String.make 1 (char_of_int (int_of_string ("0x"^n))))^str lexbuf }
  | "x" _ _	{
      raise_err lexbuf "\\x used with no following hex digits" }

and comment = parse
  | '\n'	{ new_line lexbuf; comment lexbuf }
  | "*/"	{ token lexbuf }
  | _   	{ comment lexbuf }
  | eof 	{ raise_err lexbuf "Unterminated comment" }
