open Lang.Rtl

type edge = Preference | Interfere

type t = (pseudo_reg, (pseudo_reg, edge) Hashtbl.t) Hashtbl.t

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

let is_neighbour v1 v2 edge graph =
  match Hashtbl.find_opt graph v1 with
  | None    -> false
  | Some n1 ->
    match Hashtbl.find_opt n1 v2 with
    | Some e -> e = edge
    | None   -> false

let is_empty graph = Hashtbl.length graph = 0

let merge v1 v2 graph =
  match v1, v2 with
  | (Real _ as v1), v2 | v2, (Real _ as v1) | v1, v2 ->
    let n1 = Hashtbl.find graph v1 in
    Hashtbl.iter (fun id e ->
      if id <> v1 then Hashtbl.replace n1 id e
    ) (Hashtbl.find graph v2);
    remove v2 graph;
    v1, v2

(* simplify find *)
let find_min_without_pref k graph =
  let aux id _ acc =
    if has_preference id graph then acc
    else
      let degree = degree id graph in
      match acc with
      | Some (_, d) -> if degree < d then Some (id, degree) else acc
      | None        -> if degree < k then Some (id, degree) else acc
  in
  Hashtbl.fold aux graph None

(* freeze find *)
let find_min k graph =
  let aux id _ acc =
    let degree = degree id graph in
    match acc with
    | Some (_, d) -> if degree < d then Some (id, degree) else acc
    | None        -> if degree < k then Some (id, degree) else acc
  in
  Hashtbl.fold aux graph None

(* coalesce find *)
let get_george_preference_edge k graph =
  let george v1 v2 =
    match v1, v2 with
    | (Pseu _ as v2), v1 | v1, (Pseu _ as v2)  ->
      Hashtbl.fold (fun r edge acc ->
        acc &&
        match r with
        | Real _ -> is_neighbour r v2 edge graph
        | Pseu _ -> degree r graph < k || is_neighbour r v2 edge graph
      ) (Hashtbl.find graph v1) true
    | (Real _ as v2), v1 (* | v1, (Real _ as v2) *) ->
      Hashtbl.fold (fun r edge acc ->
        acc &&
        match r with
        | Pseu _ -> is_neighbour r v2 edge graph
        | Real _ -> degree r graph < k || is_neighbour r v2 edge graph
      ) (Hashtbl.find graph v1) true
  in
  Hashtbl.fold (fun v1 neig acc ->
    match acc with
    | Some _ -> acc
    | None   ->
      Hashtbl.fold (fun v2 e acc ->
        match acc with
        | Some _ -> acc
        | None   ->
          if Preference = e &&  george v1 v2 then Some (v1, v2) else acc
      ) neig None
  ) graph None

