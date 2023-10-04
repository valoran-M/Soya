open Format
open Lang

let print_reg ppf (reg : Rtl.reg) =
  fprintf ppf "x%d" reg

let print_global ppf globals =
  let rec aux ppf globals =
    match globals with
    | []     -> ()
    | g :: l -> fprintf ppf "@ %s%a" g aux l
  in
  fprintf ppf "Globals:@[<v 2>%a@]@.@." aux globals

let print_function ppf (f : Rtl.function_def) =
  let rec print_arg ppf args =
    match args with
    | []        -> ()
    | [ a ]     -> fprintf ppf "%a" print_reg a
    | a :: args -> fprintf ppf "%a, %a" print_reg a print_arg args
  in
  let print_trans ppf id next_id =
    if id - 1 <> next_id then fprintf ppf "\tgoto %d\n" next_id
  in
  let print_instruction ppf (inst : Rtl.instruction) id =
    fprintf ppf "%5d:\t" id;
    match inst with
    | Rtl.INop n ->
      if n = id + 1
      then fprintf ppf "nop\n"
      else fprintf ppf "goto %d\n" n
    | Rtl.IPutchar (reg, next) ->
      fprintf ppf "putchar %a\n" print_reg reg;
      print_trans ppf id next
    | Rtl.IMove (r1, r2, n) ->
      fprintf ppf "move %a %a\n" print_reg r1 print_reg r2;
      print_trans ppf id n
    | Rtl.IOp (op, args, rd, n) ->
      fprintf ppf "%a = %a\n" print_reg rd
        (PrintOp.print_op print_reg) (op, args);
      print_trans ppf id n
    | Rtl.ILoad (addr, rd, n) -> 
      fprintf ppf "%a -> %a\n" PrintOp.print_addr addr print_reg rd;
      print_trans ppf id n
    | Rtl.IStore (addr, rd, n) ->
      fprintf ppf "%a -> %a\n" print_reg rd PrintOp.print_addr addr;
      print_trans ppf id n
    | Rtl.ICall (fun_id, args, n) ->
      fprintf ppf "\"%s\"(%a)\n" fun_id print_arg args;
      print_trans ppf id n
    | Rtl.ICond (c, args, nt, nf) ->
      fprintf ppf "if %a then goto %d else goto %d\n"
        (PrintOp.print_cond print_reg) (c, args) nt nf;
    | Rtl.IReturn (Some r) ->
      fprintf ppf "return %a\n" print_reg r
    | Rtl.IReturn None ->
      fprintf ppf "return\n"
    | Rtl.IGoto n ->
      fprintf ppf "goto %d\n" n
    in
  let ret_f =
    List.rev (List.sort (fun (id1, _) (id2, _) -> Int.compare id1 id2)
    (List.of_seq (Hashtbl.to_seq f.code)))
  in
  fprintf ppf "%s(%a) {\n" f.name print_arg f.params;
  print_trans ppf f.entry (match ret_f with (id1, _) :: _ -> (id1 - 1) | _ -> -1);
  List.iter (fun (id, inst) -> print_instruction ppf inst id) ret_f;
  fprintf ppf "}\n\n"

let print_prog ppf (functions : Rtl.function_def list) =
  List.iter (fun f -> fprintf ppf "%a@." print_function f) functions

let print_rtl (prog : Rtl.program) file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  print_global outf prog.globals;
  print_prog outf prog.functions

