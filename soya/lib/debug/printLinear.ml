open Format
open Lang.Linear

let print_reg ppf r =
  fprintf ppf "%s" r

let print_global ppf globals =
  let rec aux ppf globals =
    match globals with
    | []     -> ()
    | g :: l -> fprintf ppf "@ %s%a" g aux l
  in
  fprintf ppf "Globals:@[<v 2>%a@]@.@." aux globals

let print_function ppf f print_reg =
  let print_instruction ppf inst =
      match inst with
      | LPutchar reg ->
        fprintf ppf "\tputchar %a\n" print_reg reg;
      | LMove (r1, r2) ->
        fprintf ppf "\t%a <- %a\n" print_reg r1 print_reg r2;
      | LOp (op, args, rd) ->
        fprintf ppf "\t%a = %a\n" print_reg rd
          (PrintOp.print_op print_reg) (op, args);
      | LLoad (addr, rd) -> 
        fprintf ppf "\t%a -> %a\n" PrintOp.print_addr addr print_reg rd;
      | LStore (addr, rd) ->
        fprintf ppf "\t%a -> %a\n" print_reg rd PrintOp.print_addr addr;
      | LPush (rd) ->
        fprintf ppf "\tpush %a\n" print_reg rd;
      | LCall (fun_id) ->
        fprintf ppf "\t\"%s\"()\n" fun_id;
      | LCond (c, args, n) ->
        fprintf ppf "\tif %a then goto %s\n"
          (PrintOp.print_cond print_reg) (c, args) n;
      | LReturn ->
        fprintf ppf "\treturn\n"
      | LGoto l ->
        fprintf ppf "\tgoto %s\n" l
      | LLabel l ->
        fprintf ppf "%s:\n" l
  in
  fprintf ppf "%s() {\n" f.name;
  List.iter (fun i -> print_instruction ppf i) f.code;
  fprintf ppf "}\n\n%!"

let print_prog ppf functions (print_reg : formatter -> 'a -> unit) =
  List.iter (fun f -> print_function ppf f print_reg) functions

let print_lin (prog : program) file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  print_global outf prog.globals;
  print_prog outf prog.functions print_reg;
  close_out out

