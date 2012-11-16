/* Mini-C Parser */
/* Li-yao Xia */

%{
  open Lexing
  open Ast

  let loc startpos endpos =
    { start_p=startpos ; end_p=endpos }

  let mk_pointer t n =
    if n = 0 then t else Point (n,t) 

  let vdec_of_var_list t { desc=n,x ; loc=loc } =
    { desc=mk_pointer t n,x ; loc=loc }

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
%right NOT INCR DECR ADDRESS /*UStar UAdd USub*/
%left ARROW DOT LBKT

%start debugexpr debuginstr prog

%type <Ast.expr> debugexpr
%type <Ast.instr> debuginstr
%type <Ast.file> prog

%%
prog:
  dec* EOF
  { List.flatten $1 }

debugexpr:
  expr EOF {$1}

debuginstr:
  instr EOF {$1}

dec:
  | vdec_list	{ List.map (fun v -> V v) $1 }
  | s=tdec | s=fdec
    { [Dec { desc=s ; loc=loc $startpos(s) $endpos(s) }] }

vdec_list:
  | ty separated_nonempty_list(COMMA,var) SEMICOLON
      { List.map (vdec_of_var_list $1) $2 }

tdec:
  | STRUCT IDENT LBRC vdec_list* RBRC SEMICOLON
    { Typ (Struct $2, List.flatten $4) }
  | UNION IDENT LBRC vdec_list* RBRC SEMICOLON
    { Typ (Union $2, List.flatten $4) }

fdec:
  t=ty count=star_count f=IDENT
    LPAR args=separated_list(COMMA,arg) RPAR
    LBRC vslist=vdec_list* ilist=instr* RBRC
    { Fct (mk_pointer t count,f,args,List.flatten vslist,ilist) }

var:
  | star_count IDENT 	{ {desc=$1,$2 ; loc=loc $startpos $endpos} }

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
    { { desc=mk_pointer $1 $2,$3 ; loc=loc $startpos $endpos } }

expr:
  | edesc { { desc=$1 ; loc=loc $startpos $endpos } }
  | LPAR expr RPAR		{ $2 }

edesc:
  | CST 			{ Cint $1 }
  | STR 			{ Cstring $1 }
  | IDENT			{ Ident $1 }
  | expr DOT IDENT		{ Dot ($1,$3) }
  | expr ARROW IDENT            {
      Dot (
      { desc=Unop (Star,$1);
        loc=loc $startpos($1) $endpos($1) },$3) }
  | expr LBKT expr RBKT		{
      Unop (Star,
        { desc=Binop (Add,$1,$3);
          loc=loc $startpos $endpos } ) }
  | expr ASSIGN expr		{ Assign ($1,$3) }
  | f=IDENT LPAR args=separated_list(COMMA,expr) RPAR
				{ Call (f,args) }
  | u=unop e=expr		{ Unop(u,e) }
  | expr INCR			{ Unop(Incrs,$1) }
  | expr DECR			{ Unop(Decrs,$1) }
  | e1=expr o=binop e2=expr	{ Binop(o,e1,e2) }
  | SIZEOF LPAR ty star_count RPAR
		{ Sizeof (if $4=0 then $3 else Point ($4,$3)) }

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
  | PLUS 	{ Add }
  | MINUS 	{ Sub }
  | STAR 	{ Mul }
  | DIV 	{ Div }
  | MOD 	{ Mod }
  | AND 	{ And }
  | OR  	{ Or }

instr:
  | expr SEMICOLON	{ Expr $1 }
  | idesc	{ Instr { desc=$1 ; loc=loc $startpos $endpos } }

idesc:
  | SEMICOLON		 		{ Nop }
  | IF LPAR expr RPAR instr		{
      If ($3,$5,Instr {desc=Nop;loc=loc $endpos $endpos}) }
  | IF LPAR expr RPAR instr ELSE instr 	{ If ($3,$5,$7) }
  | WHILE LPAR expr RPAR instr		{ While ($3,$5) }
  | FOR LPAR init=separated_list(COMMA,expr) SEMICOLON
      test=expr? SEMICOLON
      inc=separated_list(COMMA,expr) RPAR i=instr
	{ For (init,test,inc,i) }
  | LBRC vdec_list* instr* RBRC 	{ Bloc (List.flatten $2,$3) }
  | RETURN expr? SEMICOLON		{ Return $2 }
 
