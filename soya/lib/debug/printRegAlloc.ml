open Format

let debug = ref false
let file = ref ""

let reg = PrintRTL.print_pseudo_reg

let first_live = ref true 
let first_alloc = ref true 
let first_def_use = ref true 

let gen_ppf fname ext =
  if !debug then (
    let file = Filename.remove_extension !file ^ ext in
    let out =
      if (ext = ".liveness" && !first_live) || (ext = ".alloc" && !first_alloc)
         || (ext = ".def_use" && !first_def_use)
      then open_out file
      else open_out_gen [Open_append] 0o644 file
    in
    if ext = ".liveness" then first_live := false;
    if ext = ".alloc" then first_alloc := false;
    if ext = ".def_use" then first_def_use := false;
    let outf = Format.formatter_of_out_channel out in
    Format.fprintf outf "\n\n%s :\n" fname;
    Some (outf, out))
  else None

let close_ppf ppf =
  match ppf with
  | Some (ppf, f) -> Format.pp_print_flush ppf (); close_out f
  | None -> ()


let print_liveness ppf live reg_fold =
  let reg r =
    match r with
    | Lang.Rtl.Pseu r -> Format.sprintf "x%d" r
    | Lang.Rtl.Real s -> Format.sprintf "%s" s
  in
  match ppf with
  | None -> ()
  | Some (ppf, _) ->
    Hashtbl.iter (fun id (in_, out) ->
      let si = reg_fold (fun r s -> Format.sprintf "%s %s" (reg r) s) in_ "" in
      let so = reg_fold (fun r s -> Format.sprintf "%s %s" (reg r) s) out "" in
      Format.fprintf ppf "%3d : %65s | %65s\n" id si so;
    ) live

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
