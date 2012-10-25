(** Mini-C Lexer **)
(** Li-yao Xia **)

{
  open Lexing
  open Parser

  exception Lexing_error of string

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

  let newline lexbuf =
    let pos = lexbuff.lex_curr_p in
    lexbuff.lex_curr_p <-
      { pos with
          pos_lnum = pos.pos_lnum + 1;
          pos_bol = pos.pos_cnum }
  
  let char_num lexbuf =
    lexbuff.lex_curr_p.pos_cnum - lexbuff.lex_curr_p.pos_bol
}

let space = [' ' '\t']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let octal = ['0'-'7']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']

let ident = (letter|'_') (letter|digit|'_')*

let integer = digit* | ("0x" hexa+)

let car = (['\032'-'\127']#['\\' '\'' '"'])
          | "\\\\" | "\\\'" | "\\\""
          | ("\\x" hexa hexa)

rule token = parse
  | '\n'	{ newline lexbuf; token lexbuf }
  | space+	{ token lexbuf }
  | ident as id	{ id_or_kwd id }
  | '*' 	{ STAR }
  | '(' 	{ LPAR }
  | ')' 	{ RPAR }
  | '[' 	{ LBKT }
  | ']' 	{ RBKT }
  | '{' 	{ LBRC }
  | '}' 	{ RBRC }
  | ';' 	{ SEMICOLON }
  | ',' 	{ COMMA }
  | '.' 	{ DOT }
  | "->"	{ ARROW }
  | '=' 	{ ASSIGN }
  | "++"	{ INCR }
  | "--" 	{ DECR }
  | '&' 	{ ADDRESS }
  | '!' 	{ NOT }
  | '+' 	{ PLUS }
  | '-' 	{ MINUS }
  | "=="	{ EQ }
  | "!="	{ NEQ }
  | '<' 	{ LT }
  | "<="	{ LEQ }
  | '>' 	{ GT }
  | "<="	{ GEQ }
  | '/' 	{ DIV }
  | '%' 	{ MOD }
  | "&&"	{ AND }
  | "||"	{ OR }
  | eof 	{ EOF }
  (* signed 32 bits integer in C vs signed 31 bit integer in OCaml...  *)
  | "0" octal+ as n 	{ CST (int_of_string ("0o"^n)) }
  | integer as n 	{ CST (int_of_string n) }
  | "\"" car* as s "\""	{ STR s }
  | "/*"	{ comment lexbuf }
  | "//" [^ '\n'] 	{ token lexbuf }
  | "//" [^ '\n'] eof	{ EOF }
  | _ as c		{
    raise (Lexing_error ("Illegal character: "^String.make c)) }

and comment = parse
  | '\n'	{ newline lexbuf; comment lexbuf }
  | "*/" 	{ token lexbuf }
  | _   	{ comment lexbuf }
  | eof 	{ raise (Lexing_error "Unterminated comment") }
