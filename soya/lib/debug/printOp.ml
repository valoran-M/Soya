open Format
open Lang.Op

let print_addr ppf print_reg addr =
  match addr with
  | Addr id         -> fprintf ppf "\"%s\"" id
  | AddrReg r       -> fprintf ppf "0(%a)" print_reg r
  | AddrOReg (i,r)  -> fprintf ppf "%d(%a)" i print_reg r
  | AddrStack i     -> fprintf ppf "%d(sp)" i
  | AddrGlobl id    -> fprintf ppf "\"%s\"" id

let print_cond print_reg ppf args =
  match args with
  | CConst n, [  ] -> fprintf ppf "%d" n
  | CEqi  n,  [r1] -> fprintf ppf "%a == %n" print_reg r1 n
  | CNeqi n,  [r1] -> fprintf ppf "%a != %n" print_reg r1 n
  | CEq,  [r1; r2] -> fprintf ppf "%a == %a" print_reg r1 print_reg r2
  | CNeq, [r1; r2] -> fprintf ppf "%a != %a" print_reg r1 print_reg r2
  | CLt,  [r1; r2] -> fprintf ppf "%a <  %a" print_reg r1 print_reg r2
  | CLe,  [r1; r2] -> fprintf ppf "%a <= %a" print_reg r1 print_reg r2
  | CGt,  [r1; r2] -> fprintf ppf "%a >  %a" print_reg r1 print_reg r2
  | CGe,  [r1; r2] -> fprintf ppf "%a >= %a" print_reg r1 print_reg r2
  | _ -> assert false

let print_op print_reg ppf args =
  match args with
  | OChar  n, []       -> fprintf ppf "%d" n
  | OConst n, []       -> fprintf ppf "%d" n
  | OLabel l, []       -> fprintf ppf "%s" l
  | OAddImm i,[r]      -> fprintf ppf "%d + %a" i print_reg r
  | OSubImm i,[r]      -> fprintf ppf "%d - %a" i print_reg r
  | OMulImm i,[r]      -> fprintf ppf "%d * %a" i print_reg r
  | ONeg,     [r]      -> fprintf ppf "-%a" print_reg r
  | ONot,     [r]      -> fprintf ppf "!%a" print_reg r
  | OAdd,     [r1; r2] -> fprintf ppf "%a + %a" print_reg r1 print_reg r2
  | OSub,     [r1; r2] -> fprintf ppf "%a - %a" print_reg r1 print_reg r2
  | OMul,     [r1; r2] -> fprintf ppf "%a * %a" print_reg r1 print_reg r2
  | OLt,      [r1; r2] -> fprintf ppf "%a < %a" print_reg r1 print_reg r2
  | OAnd,     [r1; r2] -> fprintf ppf "%a & %a" print_reg r1 print_reg r2
  | OOr,      [r1; r2] -> fprintf ppf "%a | %a" print_reg r1 print_reg r2
  | _, _ -> assert false

