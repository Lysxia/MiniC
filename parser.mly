/* Mini-C Parser */
/* Li-yao Xia */

%{
  open Ast
  
  (* *********************IGNORE*******************************
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
  *************************************************************)

  let make_e startpos endpos e =
    { edesc=e ;
      e_start=startpos ;
      e_end=endpos }

  let make_i startpos endpos i =
    Loc {
      idesc=i ;
      i_start=startpos ;
      i_end=endpos }

  let make_s startpos endpos s =
    { sdesc=s ;
      s_start=startpos ;
      s_end=endpos }

%}

%token <int> CST
%token <string> IDENT STR
%token CHAR ELSE FOR IF INT RETURN SIZEOF STRUCT UNION VOID WHILE
%token STAR LPAR RPAR LBKT RBKT LBRC RBRC EOF
%token SEMICOLON COMMA DOT ARROW ASSIGN
%token INCR DECR ADDRESS NOT PLUS MINUS
%token EQ NEQ LT LEQ GT GEQ DIV MOD AND OR

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR DIV MOD
%right NOT INCR DECR ADDRESS /*UStar UPlus UMinus*/
%left ARROW DOT

%start prog

%type <Ast.file> prog

%%
prog:
  statement*
  EOF
  { List.rev $1 }

statement:
  | vstmt	{ make_s $startpos $endpos (Vars $1) }
  | s=tstmt
  | s=fstmt 	{ s }

vstmt:
  vdesc { { vdesc=$1 ; v_start=$startpos ; v_end=$endpos } }

vdesc:
  ty separated_nonempty_list(COMMA,var) SEMICOLON
    { $1,$2 }

tstmt:
  | STRUCT IDENT LBRC vstmt* RBRC SEMICOLON
    { make_s $startpos $endpos
        (Typ (Struct $2, $4)) }
  | UNION IDENT LBRC vstmt* RBRC SEMICOLON
    { make_s $startpos $endpos
        (Typ (Union $2, $4)) }

fstmt:
  t=ty count=star_count f=IDENT LPAR args=separated_list(COMMA,arg) RPAR
    LBRC vslist=vstmt* ilist=instr* RBRC
    { make_s $startpos $endpos
        (Fct ((if count=0 then t else Point (count,t)),f,args,vslist,ilist)) }

var:
  star_count IDENT 	{ $1,$2 }

star_count:
  | /*EMPTY*/ 		{ 0 }
  | star_count STAR	{ $1+1 }

ty:
  | VOID 		{ Void }
  | INT 		{ Int }
  | CHAR 		{ Char }
  | STRUCT IDENT 	{ Struct $2 }
  | UNION IDENT 	{ Union $2 }

arg:
  ty star_count IDENT
    { (if $2 = 0 then $1 else Point ($2,$1)),$3 }

expr:
  edesc { { edesc=$1 ; e_start=$startpos ; e_end=$endpos } }

edesc:
  | CST 		{ Cint $1 }
  | STR 		{ Cstring $1 }
  | IDENT		{ Ident $1 }
  | expr DOT IDENT	{ Dot ($1,$3) }
  | expr ARROW IDENT	{ Dot ((Unop (Star,$1)),$3) }
  | expr LBKT expr RBKT	{ Unop (Star, Binop (Plus,$1,$3)) }
  | expr ASSIGN expr	{ Assign ($1,$3) }
  | f=IDENT LPAR args=separated_list(COMMA,expr) RPAR
			{ Call (f,args) }
  | u=unop e=expr		{ Unop(u,e) }
  | expr INCR		{ Unop(Incrs,$1) }
  | expr DECR		{ Unop(Decrs,$1) }
  | e1=expr o=binop e2=expr	{ Binop(o,e1,e2) }
  | SIZEOF LPAR ty star_count RPAR
		{ Sizeof (if $4=0 then $3 else Point ($4,$3)) }
  | LPAR expr RPAR	{ $2 }

%inline unop:
  | INCR	{ Incrp }
  | DECR	{ Decrp }
  | ADDRESS	{ Address }
  | NOT 	{ Not }
  | PLUS 	{ Uplus }
  | MINUS	{ Uminus }
  | STAR 	{ Star }

%inline binop:
  | EQ  	{ Eq }
  | NEQ 	{ Neq }
  | LT  	{ Lt }
  | LEQ 	{ Leq }
  | GT  	{ Gt }
  | GEQ 	{ Geq }
  | PLUS 	{ Plus }
  | MINUS 	{ Minus }
  | STAR 	{ Mul }
  | DIV 	{ Div }
  | MOD 	{ Mod }
  | AND 	{ And }
  | OR  	{ Or }

instr:
  | SEMICOLON 		{ make_i $startpos $endpos Nop }
  | expr SEMICOLON 	{ make_i $startpos $endpos (Expr $1) }
  | IF LPAR expr RPAR instr		{ make_i $startpos $endpos
					    (If ($3,$5,Instr Nop)) }
  | IF LPAR expr RPAR instr ELSE instr	{ make_i $startpos $endpos
                                            (If ($3,$5,$7)) }
  | WHILE LPAR expr RPAR instr		{ make_i $startpos $endpos
                                            (While ($3,$5)) }
  | FOR LPAR init=separated_list(COMMA,expr) SEMICOLON
      test=expr? SEMICOLON
      inc=separated_list(COMMA,expr) RPAR i=instr
	{ make_i $startpos $endpos (Bloc ([],
            (List.map (fun e -> Instr (Expr e)) init)@
            [ Instr
            (
            While ((match test with
                     | None ->
                         make_e $startpos(test) $endpos(test) (Cint 1)
                     | Some e -> e),
                   (make_i $startpos(inc) $endpos(inc) (Bloc ([],
                      (List.map
                         (fun e -> Instr (Expr e)) inc)@
                      [i])))
                  )
            )
            ])) }
  | LBRC vstmt* instr* RBRC 		{ make_i $startpos $endpos
					    (Bloc ($2,$3)) }
  | RETURN expr? SEMICOLON		{ make_i $startpos $endpos
					    (Return $2) }

