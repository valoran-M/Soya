open Format
open Lang.Rtl

let print_addr ppf addr =
  match addr with
  | Addr id      -> fprintf ppf "\"%s\"" id
  | AddrStack i  -> fprintf ppf "%d(sp)" i
  | AddrGlobl id -> fprintf ppf "\"%s\"" id

let print_cond print_reg ppf args =
  match args with
  | CEqi  n,  [r1] -> fprintf ppf "%a == %n" print_reg r1 n
  | CNeqi n,  [r1] -> fprintf ppf "%a != %n" print_reg r1 n
  | CEq,  [r1; r2] -> fprintf ppf "%a == %a" print_reg r1 print_reg r2
  | CNeq, [r1; r2] -> fprintf ppf "%a != %a" print_reg r1 print_reg r2
  | CLt,  [r1; r2] -> fprintf ppf "%a < %a" print_reg r1 print_reg r2
  | _ -> assert false

let print_op print_reg ppf args =
  match args with
  | OConst n, []       -> fprintf ppf "%d" n
  | OAdd,     [r1; r2] -> fprintf ppf "%a + %a" print_reg r1 print_reg r2
  | OMul,     [r1; r2] -> fprintf ppf "%a * %a" print_reg r1 print_reg r2
  | OLt,      [r1; r2] -> fprintf ppf "%a < %a" print_reg r1 print_reg r2
  | _, _ -> assert false

