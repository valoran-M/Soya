open Format
open Lang.Ltl

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
  let see = Hashtbl.create 16 in
  let rec print_instruction ppf id =
    if not (Hashtbl.mem see id) then (
      Hashtbl.add see id ();
      fprintf ppf "%5d:\t" id;
      match Hashtbl.find f.code id with
      | IPutchar (reg, n) ->
        fprintf ppf "putchar %a\n" print_reg reg;
        print_instruction ppf n
      | IMove (r1, r2, n) ->
        fprintf ppf "%a <- %a\n" print_reg r1 print_reg r2;
        print_instruction ppf n
      | IOp (op, args, rd, n) ->
        fprintf ppf "%a = %a\n" print_reg rd
          (PrintOp.print_op print_reg) (op, args);
        print_instruction ppf n
      | ILoad (addr, rd, n) -> 
        fprintf ppf "%a -> %a\n" PrintOp.print_addr addr print_reg rd;
        print_instruction ppf n
      | IStore (addr, rd, n) ->
        fprintf ppf "%a -> %a\n" print_reg rd PrintOp.print_addr addr;
        print_instruction ppf n
      | IPush (rd, n) ->
        fprintf ppf "push %a\n" print_reg rd;
        print_instruction ppf n
      | ICall (fun_id, _, n) ->
        fprintf ppf "\"%s\"()\n" fun_id;
        print_instruction ppf n
      | ICond (c, args, nt, nf) ->
        fprintf ppf "if %a then goto %d else goto %d\n"
          (PrintOp.print_cond print_reg) (c, args) nt nf;
        print_instruction ppf nt;
        print_instruction ppf nf
      | IReturn (Some r) ->
        fprintf ppf "return %a\n" print_reg r
      | IReturn None ->
        fprintf ppf "return\n"
      | IGoto n ->
        fprintf ppf "goto %d\n" n;
        print_instruction ppf n)
    in
  fprintf ppf "%s() {\n" f.name;
  print_instruction ppf f.entry;
  fprintf ppf "}\n\n%!"

let print_prog ppf functions (print_reg : formatter -> 'a -> unit) =
  List.iter (fun f -> print_function ppf f print_reg) functions

let print_ltl (prog : program) file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  print_global outf prog.globals;
  print_prog outf prog.functions print_reg;
  pp_print_flush outf ();
  close_out out;

