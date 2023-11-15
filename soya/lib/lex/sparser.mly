%{

  open Lexing
  open Lang.Soya

  let classes = ref []
  let globals = ref []
  let functions = ref []

  let mk_loc (fc, lc) = 
    {fc = fc; lc = lc}

  let mk_expr loc e =
    { annot = mk_loc loc; expr = e }

  let set_parent p loc =
    match p with
    | Some p -> Some (p, loc)
    | None -> None

%}

%token SUB PLUS STAR
%token LT LE GT GE EQ NEQ AND OR NOT

%token <char> CHAR
%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token TYP_INT TYP_BOOL TYP_VOID TYP_CHAR
%token VAR FUNCTION ABSTRACT
%token ATTRIBUTE METHOD STATIC EXTENDS CLASS THIS SUPER
%token DOT NEW LBRACKET RBRACKET
%token LPAR RPAR BEGIN END COMMA SEMI
%token PUTCHAR SET IF ELSE WHILE RETURN INSTANCEOF
%token EOF

%left     OR
%left     AND
%nonassoc NOT

%left     EQ NEQ
%left     LT LE GT GE
%left     INSTANCEOF
%left     PLUS SUB 
%left     STAR
%nonassoc LBRACKET DOT

%start program
%type <location program> program

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
| c=class_def     { classes := c :: !classes }
| v=variable_decl { let id, ty = v in globals := (id, ty) :: !globals }
| f=function_def  { functions := f :: !functions }
;


class_def:
| CLASS name=IDENT parent=option(EXTENDS p=IDENT { p })
    BEGIN fields=list(attribute_decl) m=methods_def END 
  { let methods, static = m in
    { name; fields; methods; static;
      parent = set_parent parent (mk_loc $loc(parent));
      abstract = false;
      loc = mk_loc $sloc;
      abs_methods = [] } }
| ABSTRACT CLASS name=IDENT parent=option(EXTENDS p=IDENT { p })
    BEGIN fields=list(attribute_decl) m=abs_methods_def END 
  { let methods, static, abs_methods  = m in
    { name; fields; methods; static;
      parent = set_parent parent (mk_loc $loc(parent));
      abstract = true;
      loc = mk_loc $sloc;
      abs_methods } }
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
| TYP_CHAR { TChar }
| TYP_INT  { TInt  }
| TYP_BOOL { TBool }
| TYP_VOID { TVoid }
| LBRACKET ty=typ RBRACKET { TArray ty }
| id=IDENT { TClass id }
;

function_def:
| FUNCTION fdef=fun_def { fdef }
;

abs_methods_def:
| {  [], [], [] }
| STATIC METHOD mdef=fun_def m=abs_methods_def
  { let m, s, a = m in m, mdef :: s, a}
| METHOD mdef=fun_def m=abs_methods_def
  { let m, s, a = m in mdef :: m, s, a}
| ABSTRACT METHOD return=typ name=IDENT
  LPAR params=separated_list(COMMA, typed_ident) RPAR SEMI a=abs_methods_def
  { let m, s, a = a in
    m, s, {name; code=[]; params; return; locals=[]} :: a }


methods_def:
| { [], [] }
| STATIC METHOD mdef=fun_def m = methods_def { fst m, mdef :: snd m }
|        METHOD mdef=fun_def m = methods_def { mdef :: fst m, snd m }
;

fun_def:
| return=typ name=IDENT LPAR params=separated_list(COMMA, typed_ident) RPAR
    BEGIN c=code END
    { let locals, code = c in
      {name; code; params; return; locals} }
;

code:
| {[], []}
| VAR tid=typed_ident SEMI c=code       { tid :: fst c,      snd c }
| i=instruction c=code                  {        fst c, i :: snd c }
| tid=typed_ident SET e=expression SEMI c=code
    { tid :: fst c, Set (fst tid, mk_loc $loc(tid), e) :: snd c }

instruction:
| PUTCHAR LPAR e=expression RPAR SEMI { Putchar(e) }
| id=IDENT SET e=expression SEMI      { Set(id, mk_loc $loc(id), e) }
| e=expression SEMI                   { Expr(e) }
| m=mem_access SET e=expression SEMI  { Write(m, e) }
| RETURN e=expression SEMI            { Return(e) }
| IF LPAR c=expression RPAR
    BEGIN s1=list(instruction) END
    ELSE BEGIN s2=list(instruction) END   { If(c, s1, s2) }
| WHILE LPAR c=expression RPAR
    BEGIN s=list(instruction) END         { While(c, s) }
;

mem_access:
| e1=expression LBRACKET e2=expression RBRACKET { Arr(e1, e2) }
| e=expression DOT id=IDENT                     { Atr(e, id, mk_loc $loc(id)) }
;

expression:
| c=CHAR                                { mk_expr $sloc (Char (Char.code c)) }
| n=CST                                 { mk_expr $sloc (Cst n) }
| b=BOOL                                { mk_expr $sloc (Bool b) }
| id=IDENT                              { mk_expr $sloc (Var id) }
| LPAR e=expression RPAR                { e }
| THIS                                  { mk_expr $sloc (This) }
| SUPER                                 { mk_expr $sloc (Super) }
| m=mem_access                          { mk_expr $sloc (Read m) }
| e1=expression op=binop e2=expression  { mk_expr $sloc (Binop(op, e1, e2)) }
| op=unop e=expression                  { mk_expr $sloc (Unop(op, e)) }
| e=expression INSTANCEOF id=IDENT
  { mk_expr $sloc (Instanceof (e, (id, mk_loc $loc(id)))) }
| f=IDENT LPAR params=separated_list(COMMA, expression) RPAR
  { mk_expr $sloc (Call(f, params)) }
| e=expression DOT f=IDENT LPAR params=separated_list(COMMA, expression) RPAR
  { mk_expr $sloc (MCall(e, f, params)) }
| SUPER LPAR params=separated_list(COMMA, expression) RPAR
  { mk_expr $sloc (MCall((mk_expr $sloc Super), "constructor", params)) }
| NEW id=IDENT LPAR params=separated_list(COMMA, expression) RPAR
  { mk_expr $sloc (New(id, params)) }
| NEW LBRACKET ty=typ COMMA e=expression RBRACKET
  { mk_expr $sloc (NewTab(ty, e)) }
;

%inline binop:
| PLUS  { Lang.Imp.Add }
| SUB   { Lang.Imp.Sub }
| STAR  { Lang.Imp.Mul }
| AND   { Lang.Imp.And }
| OR    { Lang.Imp.Or }
| LT    { Lang.Imp.Lt }
| LE    { Lang.Imp.Le }
| GT    { Lang.Imp.Gt }
| GE    { Lang.Imp.Ge }
| EQ    { Lang.Imp.Eq }
| NEQ   { Lang.Imp.Neq }
;

%inline unop:
| NOT { Lang.Imp.Not }
| SUB { Lang.Imp.Neg }
;
