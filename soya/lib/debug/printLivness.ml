open Format
open Lang.Rtl

let print_pseudo_reg ppf reg =
  match reg with
  | Pseu r -> fprintf ppf "x%d" r
  | Real s -> fprintf ppf "%s" s

let print_livness a file ext =
  let file = Filename.remove_extension file ^ ext in
  let out = open_out file in
  let _outf = formatter_of_out_channel out in
  let _ret_f =
    List.sort (fun (id1, _) (id2, _) -> Int.compare id2 id1)
      (List.of_seq (Hashtbl.to_seq a))
  in
  ()

  (* List.iter (fun (id, (in_, out)) -> *)
  (*   fprintf outf "%d : " id; *)
  (*   Translate.Regalloc.Reg_set.iter (fun reg -> *)
  (*     fprintf outf "%a " print_pseudo_reg reg *)
  (*   ) in_; *)
  (*   fprintf outf "\\ "; *)
  (*   Translate.Regalloc.Reg_set.iter (fun reg -> *)
  (*     fprintf outf "%a " print_pseudo_reg reg *)
  (*   ) out; *)
  (*   fprintf outf "\n"; *)
  (* ) ret_f; *)

