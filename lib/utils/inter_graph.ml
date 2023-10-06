open Lang.Rtl

type edge = Preference | Interfere

type t = (reg, (reg, edge) Hashtbl.t) Hashtbl.t

let create () = Hashtbl.create 32

let add v1 v2 edge (g: t) =
  let add_aux id1 id2 =
    match Hashtbl.find_opt g id1 with
    | None ->
      let neig = Hashtbl.create 8 in
      Hashtbl.replace neig id2 edge;
      Hashtbl.replace g id1 neig
    | Some neig ->
      match Hashtbl.find_opt neig id2 with
      | Some Interfere -> ()
      | _ -> Hashtbl.replace neig id2 edge
  in
  add_aux v1 v2;
  add_aux v2 v1

let remove_edge v1 v2 graph =
  match Hashtbl.find_opt graph v1, Hashtbl.find_opt graph v2 with
  | Some n1, Some n2 -> Hashtbl.remove n1 v2; Hashtbl.remove n2 v1
  | _, _ -> ()

let remove_pref_edge v graph =
  match Hashtbl.find_opt graph v with
  | None -> ()
  | Some neig ->
    Hashtbl.iter (fun v' p ->
      match p with
      | Preference -> remove_edge v v' graph
      | _ -> ()) neig

let remove v graph =
  match Hashtbl.find_opt graph v with
  | None -> ()
  | Some neig ->
    Hashtbl.iter (fun id _ -> remove_edge id v graph) neig;
    Hashtbl.remove graph v

let degree v graph =
  match Hashtbl.find_opt graph v with
  | None   -> 0
  | Some n -> Hashtbl.length n

let has_preference v graph =
  match Hashtbl.find_opt graph v with
  | None   -> false
  | Some n -> Hashtbl.fold (fun _ p a -> a || p = Preference) n false

let is_empty graph = Hashtbl.length graph = 0

