%{
  open Lang
  open Lexing
  open Imp

%}

%token PLUS STAR
%token LT

%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token VAR FUNCTION COMMA
%token LPAR RPAR BEGIN END SEMI
%token PUTCHAR SET IF ELSE WHILE RETURN
%token EOF

%left LT
%left PLUS
%left STAR

%start program
%type <Imp.program> program

%%

program:
| globals=list(variable_decl) functions=list(function_def) EOF
    { {functions; globals} }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

variable_decl:
| VAR id=IDENT SEMI { id }
;

function_def:
| FUNCTION name=IDENT LPAR params=separated_list(COMMA, IDENT) RPAR
    BEGIN locals=list(variable_decl) code=list(instruction) END
    { {name; code; params; locals} }
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
;

expression:
| n=CST { Cst(n) }
| b=BOOL { Bool(b) }
| id=IDENT { Var(id) }
| LPAR e=expression RPAR { e }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| f=IDENT LPAR params=separated_list(COMMA, expression) RPAR { Call(f, params) }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| LT { Lt }
;
