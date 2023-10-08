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
  let print_trans ppf id next_id =
    if id - 1 <> next_id then fprintf ppf "\tgoto %d\n" next_id
  in
  let print_instruction ppf inst id =
    fprintf ppf "%5d:\t" id;
    match inst with
    | INop n ->
      if n = id + 1
      then fprintf ppf "nop\n"
      else fprintf ppf "goto %d\n" n
    | IPutchar (reg, next) ->
      fprintf ppf "putchar %a\n" print_reg reg;
      print_trans ppf id next
    | IMove (r1, r2, n) ->
      fprintf ppf "%a <- %a\n" print_reg r1 print_reg r2;
      print_trans ppf id n
    | IOp (op, args, rd, n) ->
      fprintf ppf "%a = %a\n" print_reg rd
        (PrintOp.print_op print_reg) (op, args);
      print_trans ppf id n
    | ILoad (addr, rd, n) -> 
      fprintf ppf "%a -> %a\n" PrintOp.print_addr addr print_reg rd;
      print_trans ppf id n
    | IStore (addr, rd, n) ->
      fprintf ppf "%a -> %a\n" print_reg rd PrintOp.print_addr addr;
      print_trans ppf id n
    | IPop (rd, n) ->
      fprintf ppf "pop %a\n" print_reg rd;
      print_trans ppf id n
    | IPush (rd, n) ->
      fprintf ppf "push %a\n" print_reg rd;
      print_trans ppf id n
    | ICall (fun_id, _, n) ->
      fprintf ppf "\"%s\"()\n" fun_id;
      print_trans ppf id n
    | ICond (c, args, nt, nf) ->
      fprintf ppf "if %a then goto %d else goto %d\n"
        (PrintOp.print_cond print_reg) (c, args) nt nf;
    | IReturn (Some r) ->
      fprintf ppf "return %a\n" print_reg r
    | IReturn None ->
      fprintf ppf "return\n"
    | IGoto n ->
      fprintf ppf "goto %d\n" n
    in
  let ret_f =
    List.sort (fun (id1, _) (id2, _) -> Int.compare id2 id1)
      (List.of_seq (Hashtbl.to_seq f.code))
  in
  fprintf ppf "%s() {\n" f.name;
  print_trans ppf f.entry (match ret_f with (id1, _) :: _ -> (id1 - 1) | _ -> -1);
  List.iter (fun (id, inst) -> print_instruction ppf inst id) ret_f;
  fprintf ppf "}\n\n%!"

let print_prog ppf functions (print_reg : formatter -> 'a -> unit) =
  List.iter (fun f -> print_function ppf f print_reg) functions

let print_ltl (prog : program) file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  print_global outf prog.globals;
  print_prog outf prog.functions print_reg

