%{

  open Lexing
  open Lang.Simp

  let structs = ref []
  let globals = ref []
  let functions = ref []

%}

%token PLUS STAR
%token LT

%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token TYP_INT TYP_BOOL TYP_VOID
%token VAR FUNCTION
%token DOT NEW LBRACKET RBRACKET STRUCT
%token LPAR RPAR BEGIN END COMMA SEMI
%token PUTCHAR SET IF ELSE WHILE RETURN
%token EOF

%left LT
%left PLUS
%left STAR
%nonassoc LBRACKET DOT

%start program
%type <program> program

%%

program:
| list(decl) EOF
    { {structs = List.rev !structs;
       functions = List.rev !functions;
       globals = List.rev !globals} }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

decl:
| s=struct_def { structs := s :: !structs }
| v=variable_decl { let id, ty = v in globals := (id, ty) :: !globals }
| f=function_def { functions := f :: !functions }
;

struct_def:
| STRUCT name=IDENT BEGIN fields=separated_list(SEMI, typed_ident) END { {name; fields} }
;

variable_decl:
| VAR tid=typed_ident SEMI { tid }
;

typed_ident:
| ty=typ id=IDENT { id, ty }
;

typ:
| TYP_INT { TInt }
| TYP_BOOL { TBool }
| TYP_VOID { TVoid }
| LBRACKET ty=typ RBRACKET { TArray ty }
| id=IDENT { TStruct id }
;

function_def:
| FUNCTION return=typ name=IDENT LPAR params=separated_list(COMMA, typed_ident) RPAR
    BEGIN locals=list(variable_decl) code=list(instruction) END
    { {name; code; params; return; locals} }
;

instruction:
| PUTCHAR LPAR e=expression RPAR SEMI { Putchar(e) }
| id=IDENT SET e=expression SEMI { Set(id, e) }
| IF LPAR c=expression RPAR
    BEGIN s1=list(instruction) END
    ELSE BEGIN s2=list(instruction) END { If(c, s1, s2) }
| WHILE LPAR c=expression RPAR
    BEGIN s=list(instruction) END { While(c, s) }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
| m=mem_access SET e=expression SEMI { Write(m, e) }
;

mem_access:
| e1=expression LBRACKET e2=expression RBRACKET { Arr(e1, e2) }
| e=expression DOT id=IDENT { Str(e, id) }
;

expression:
| n=CST { Cst n }
| b=BOOL { Bool b }
| id=IDENT { Var id }
| LPAR e=expression RPAR { e }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| f=IDENT LPAR params=separated_list(COMMA, expression) RPAR { Call(f, params) }
| NEW id=IDENT { New(id) }
| NEW LBRACKET ty=typ COMMA e=expression RBRACKET { NewTab(ty, e) }
| m=mem_access { Read m }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| LT { Lt }
;
