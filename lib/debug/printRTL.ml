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
  fprintf ppf "%s(%a) {\n}\n\n" f.name print_arg f.params

let print_prog ppf (functions : Rtl.function_def list) =
  List.iter (fun f -> fprintf ppf "%a@." print_function f) functions

let print_rtl (prog : Rtl.program) file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  print_global outf prog.globals;
  print_prog outf prog.functions
  
