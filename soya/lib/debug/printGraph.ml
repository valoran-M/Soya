open Format
open Lang.Rtl

let print_pseudo_reg ppf reg =
  match reg with
  | Pseu r -> fprintf ppf "x%d" r
  | Real s -> fprintf ppf "%s" s

let print_graph a file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in

  let e = Hashtbl.create 16 in
  fprintf outf "strict graph in {\n";
  Hashtbl.iter (fun r1 neig -> 
    Hashtbl.iter (fun r2 p ->
      Hashtbl.replace e (r2, r1) ();
      (* if not (Hashtbl.mem e (r1, r2)) then *)
        fprintf outf "\t\"%a\" -- \"%a\" %s;\n"
      print_pseudo_reg r1 print_pseudo_reg r2
      (if p = Utils.Interference_graph.Preference
      then "[style=dotted]"
      else "")
    ) neig
  ) a;
  fprintf outf "}\n";
  pp_print_flush outf ();
  close_out out

