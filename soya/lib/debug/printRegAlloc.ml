open Format

let reg = PrintRTL.print_pseudo_reg

let first = ref true

let gen_ppf debug file fname =
  if debug then (
    let out =
      if !first
      then open_out file
      else open_out_gen [Open_append] 0o644 file
    in
    first := false;
    let outf = Format.formatter_of_out_channel out in
    Format.fprintf outf "\n\n%s :\n" fname;
    Some (outf, out))
  else None

let simplify_select ppf v =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\tsimplify (%a)\n" reg v
  | None -> ()

let simplify_coalesce ppf v1 v2 =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\tcoalesce (%a -- %a)\n" reg v1 reg v2
  | None -> ()

let select_reg ppf v1 c =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\tselected (%a) <- %s\n" reg v1 c
  | None -> ()

let spill_reg ppf v =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\tspill (%a)\n" reg v
  | None -> ()

let spilled_reg ppf v =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\tspilled (%a)\n" reg v
  | None -> ()

let remove_pref ppf v =
  match ppf with
  | Some (ppf, _) -> fprintf ppf "\trm_pref (%a)\n" reg v
  | None -> ()
