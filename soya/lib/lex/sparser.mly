%{

  open Lexing
  open Lang.Soya

  let classes = ref []
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
%token ATTRIBUTE METHOD EXTENDS CLASS THIS
%token DOT NEW LBRACKET RBRACKET
%token LPAR RPAR BEGIN END COMMA SEMI
%token PUTCHAR SET IF ELSE WHILE RETURN
%token EOF

%left LT
%left PLUS
%left STAR
%nonassoc LBRACKET DOT

%start program
%type <unit program> program

%%

program:
| list(decl) EOF
    { {classes = List.rev !classes;
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
| c=class_def { classes := c :: !classes }
| v=variable_decl { let id, ty = v in globals := (id, ty) :: !globals }
| f=function_def { functions := f :: !functions }
;


class_def:
| CLASS name=IDENT parent=option(EXTENDS p=IDENT { p }) 
   BEGIN fields=list(attribute_decl) methods=list(method_def) END 
   { { name; fields; methods; parent; } }
;

variable_decl:
| VAR tid=typed_ident SEMI { tid }
;

attribute_decl:
| ATTRIBUTE tid=typed_ident SEMI { tid }
;  

typed_ident:
| ty=typ id=IDENT { id, ty }
;

typ:
| TYP_INT { TInt }
| TYP_BOOL { TBool }
| TYP_VOID { TVoid }
| LBRACKET ty=typ RBRACKET { TArray ty }
| id=IDENT { TClass id }
;

function_def:
| FUNCTION fdef=fun_def { fdef }
;

method_def:
| METHOD mdef=fun_def { mdef }
;

fun_def:
| return=typ name=IDENT LPAR params=separated_list(COMMA, typed_ident) RPAR
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
| e=expression DOT id=IDENT { Atr(e, id) }
;

expression:
| n=CST { mk_expr () (Cst n) }
| b=BOOL { mk_expr () (Bool b) }
| id=IDENT { mk_expr () (Var id) }
| LPAR e=expression RPAR { e }
| e1=expression op=binop e2=expression { mk_expr () (Binop(op, e1, e2)) }
| f=IDENT LPAR params=separated_list(COMMA, expression) RPAR { mk_expr () (Call(f, params)) }
| e=expression DOT f=IDENT LPAR params=separated_list(COMMA, expression) RPAR { mk_expr () (MCall(e, f, params)) }
| NEW id=IDENT LPAR params=separated_list(COMMA, expression) RPAR { mk_expr () (New(id, params)) }
| NEW LBRACKET ty=typ COMMA e=expression RBRACKET { mk_expr () (NewTab(ty, e)) }
| m=mem_access { mk_expr () (Read m) }
| THIS { mk_expr () (This) }
;

%inline binop:
| PLUS { Lang.Imp.Add }
| STAR { Lang.Imp.Mul }
| LT { Lang.Imp.Lt }
;

