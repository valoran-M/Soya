open Format

let print_graph graph file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  fprintf outf "graph interference {\n";
  Hashtbl.iter (fun (id1, id2) v ->
    match v with
    | Translate.Regalloc.Preference ->
      fprintf outf "\t\"%s\" -- \"%s\" [style=dotted]\n" id1 id2
    | Translate.Regalloc.Interfere ->
      fprintf outf "\t\"%s\" -- \"%s\"\n" id1 id2
  ) graph;
  fprintf outf "}\n"


