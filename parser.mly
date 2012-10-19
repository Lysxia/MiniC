/* Mini-C Parser */
/* Li-yao Xia */

%{
  open Ast
  
  /**********************IGNORE*******************************
  /* Variables are translated into numbers w_hile parsing */
  /* Unbound variables are detected a_nd r_aise an e_xception*/
  /* d_one because I dislike that syntaxic coloring o_f comments */
  /* Local environment */
  module Env = Map.Make(String)

  /* Bound s_truct/union field names */
  let su_field = Hashtbl.create 37
  
  let su_find =
    Hashtbl.find su_field

  let su_add x =
    try (Hashtbl.find su_field x)
      with _ ->
        Hashtbl.add su_field x (Hashtbl.length su_field)
  
  /* Bound global variable names */
  let global = Hashtbl.create 37

  let g_find =
    Hashtbl.find global
  
  let g_add x =
    try (Hashtbl.find global x)
      with _ ->
        Hashtbl.add global x (Hashtbl.length global)

  /* Return following unassigned integer */
  let g_num () = Hashtbl.length global
  *************************************************************/
%}

%token <int> CST
%token <string> IDENT STR
%token CHAR ELSE FOR IF INT RETURN SIZEOF STRUCT UNION VOID WHILE
%token STAR LPAR RPAR LBKT RBKT LBRC RBRC EOF
%token SEMICOLON COMMA DOT ARRW ASSIGN
%token INCR DECR ADDRESS NOT PLUS MINUS
%token OR EQ NEQ LT LEQ GT GEQ DIV MOD AND OR

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR DIV MOD
%right NOT INCR DECR ADDRESS ustar uplus uminus
%left ARROW DOT

%start prog

%type <Ast.file> prog

%%

prog:
  statement*
  EOF
  { List.flatten $1 }

statement:
  | s=vstmt | s=tstmt | s=fstmt { s }

vstmt:
  t=ty separated_nonempty_list(COMMA, var(t)) SEMICOLON
    { Vars $2 }

tstmt:
  | STRUCT IDENT LBRC vstmt* RBRC SEMICOLON
    { Typ (Struct $2, $4) }
  | UNION IDENT LBRC vstmt* RBRC SEMICOLON
    { Typ (Union $2, $4) }

fstmt:
  t=ty var(t) LPAR separated_list(COMMA, arg) RPAR block
    { Fct ($2,$4,arg,block) }

ty:
  | VOID { Void }
  | INT { Int }
  | CHAR { Char }
  | STRUCT { Struct $1 }
  | UNION { Union $1 }

arg:
  t=ty var(t)
    { $2 }

var(t):
  | STAR var(t) %prec ustar { Point $2 }
  | IDENT  { t, Name $1 }

pnt:
  | STAR pnt %prec ustar { match $2 with
    | Name s -> Pnt (1,s)
    | Pnt n,s -> Pnt (n+1,s) }
  | IDENT  { Name $1 }

expr:
  | SEMICOLON { assert false }

%inline binop:
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LEQ { Leq }
  | GT { Gt }
  | GEQ { Geq }
  | PLUS { Plus }
  | MINUS { Minus }
  | STAR { Mul }
  | DIV { Div }
  | MOD { Mod }
  | AND { And }
  | OR { Or }

instr:
  | SEMICOLON { }

block:
