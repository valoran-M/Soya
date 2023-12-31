open Lang.Soya

let rec merge_fields envc c acc =
  match Hashtbl.find_opt envc c with
  | None -> assert false
  | Some c ->
    let acc = List.append c.fields acc in
    match c.parent with
    | None -> acc
    | Some (c, _) -> merge_fields envc c acc

let rec merge_methods envm (prog : typ program) (c: typ class_def) =
  match Hashtbl.find_opt envm c.name with
  | Some m -> m
  | None ->
    let cm = List.fold_right (fun m l -> (c.name, m) :: l) c.methods [] in
    match c.parent with
    | None -> Hashtbl.add envm c.name cm; cm
    | Some (p, _) ->
      let p = List.find (fun c -> c.name = p) prog.classes in
      let m = merge_methods envm prog p in
      let m = List.fold_left
        (fun (nm:(string * typ function_def) list) (_,(m:typ function_def)) ->
          match List.find_opt
            (fun (_, (cm : typ function_def)) -> cm.name = m.name) nm with
          | None -> (p.name , m) :: nm
          | Some (_, rm) ->
            (c.name, rm) ::
            (List.filter (fun (_,(m:'a function_def)) -> m.name <> rm.name) nm)
        ) cm m in
      Hashtbl.add envm c.name m; m

let rec get_method envc c m mloc =
  let c = Hashtbl.find envc c in
  match List.find_opt (fun (e : 'a function_def) -> e.name = m) c.methods with
  | Some m -> m
  | None   ->
    match c.parent with
    | Some (p, _) -> get_method envc p m mloc
    | None        -> Error_soy.Error.undeclared_methode mloc m

let get_static_method envc c m mloc =
  let c = Hashtbl.find envc c in
  match List.find_opt (fun (e : 'a function_def) -> e.name = m) c.static with
  | Some m -> m
  | None   -> Error_soy.Error.undeclared_methode mloc m

let rec get_field envc c f loc =
  let c = Hashtbl.find envc c in
  match List.find_opt (fun (e, _) -> e = f) c.fields with
  | Some f -> f
  | None   ->
    match c.parent with
    | Some (p, _) -> get_field envc p f loc
    | None        -> Error_soy.Error.undeclared_field loc f

