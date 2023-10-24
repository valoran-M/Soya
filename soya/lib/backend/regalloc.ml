open Utils
open Lang.Rtl
open Lang.Mips

module Reg_set = Set.Make(
  struct
    type t = pseudo_reg
    let compare t1 t2 =
      match (t1, t2) with
      | Real r1, Real r2   -> String.compare r1 r2
      | Pseu r1, Pseu r2 -> Int.compare r1 r2
      | Pseu _,  Real _    -> -1
      | Real _,  Pseu _  ->  1

  end
)

let k = List.length Regs.register

(* Liveness ----------------------------------------------------------------- *)

let get_liveness (rtl_fun : pseudo_reg function_def) =
  (* utilisation number of register *)
  let reg_nb_use = Hashtbl.create 32 in
  let incr_reg r =
    match Hashtbl.find_opt reg_nb_use r, r with
    | Some n, Pseu _ -> Hashtbl.replace reg_nb_use r (n+1)
    | None,   Pseu _ -> Hashtbl.replace reg_nb_use r 1
    | _ -> ()
  in
  (* Def and Use array *)
  let def_use = Hashtbl.create 32 in
  let add_def_use id def use =
    Hashtbl.replace def_use id (Reg_set.of_list def, Reg_set.of_list use)
  in

  (* pred and succ array *)
  let pred_succ = Hashtbl.create 32 in
  (* Hashtbl.iter (fun id _ -> *)
  (*   Printf.printf "%d " id; *)
  (*   Hashtbl.replace pred_succ id ([], [])) rtl_fun.code; *)
  (* print_newline (); *)
  let add_pred id pred =
    if pred >= 0 then (
      (* Printf.printf "%d\n" id; *)
      match Hashtbl.find_opt pred_succ id with
      | None        -> Hashtbl.replace pred_succ id ([pred],    [])
      | Some (p, s) -> Hashtbl.replace pred_succ id (pred :: p, s ))
  in
  let add_succ id succ =
    match Hashtbl.find_opt pred_succ id with
    | None        -> Hashtbl.replace pred_succ id ([], [succ]);
    | Some (p, s) -> Hashtbl.replace pred_succ id (p,  succ :: s)
  in

  let addr_use = function
    | Lang.Op.AddrReg r -> [r]
    | _ -> []
  in

  (* Init data (in/out and pred/succ) for get liveness *)
  let rec init pred id =
    add_pred id pred;
    if not (Hashtbl.mem def_use id) then (
      match Hashtbl.find rtl_fun.code id with
      | INop n -> add_succ id n; init id n
      | IPutchar (r, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [] [Real v0; Real a0];
        init id n
      | IAlloc (r, _, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [] [Real v0; Real a0];
        init id n
      | IMove (rd, r, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [rd] [r];
        init id n
      | IOp (_, args, rd, n) ->
        incr_reg rd; List.iter incr_reg args;
        add_succ id n;
        add_def_use id [rd] args;
        init id n
      | ILoad (AddrReg r, rd, n) ->
        incr_reg r; incr_reg rd;
        add_succ id n;
        add_def_use id [rd] [r];
        init id n
      | ILoad (_, rd, n) ->
        incr_reg rd;
        add_succ id n;
        add_def_use id [rd] [];
        init id n
      | IStore (a, r, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [] (r :: addr_use a);
        init id n
      | IGetParam (r, _, _, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [r] [];
        init id n
      | ISetParam (r, _, _, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [] [r];
        init id n
      | ICall (_, _, nb_args, _, n) ->
        add_succ id n;
        add_def_use id (Real ra :: Regs.caller_saved)
          (Real v0 :: Util.get_k_first (min 4 nb_args)
            [Real a0; Real a1; Real a2; Real a3]);
        init id n
      | ICond (_, args, nt, nf) ->
        List.iter incr_reg args;
        add_succ id nt; add_succ id nf;
        add_def_use id [] args;
        init id nt;
        init id nf
      | IReturn (Some r) ->
        incr_reg r;
        add_def_use id [] (Real v0 :: Regs.callee_saved)
      | IReturn None ->
        add_def_use id [] (Real v0 :: Regs.callee_saved)
      | IGoto n ->
        add_succ id n;
        init id n)
  in

  (* In Out array *)
  let in_out = Hashtbl.create 32 in
  let add id in_ out_ =
    Hashtbl.replace in_out id (in_, out_)
  in
  Hashtbl.iter (fun id _ -> add id Reg_set.empty Reg_set.empty) rtl_fun.code;

  (* liveness for liveness fixpoint *)
  let file = Queue.create () in
  Hashtbl.iter (fun id _ -> Queue.add id file) rtl_fun.code;

  let rec liveness_assgn () =
    match Queue.take_opt file with
    | None    -> ()
    | Some id ->
      let old_in, _ = Hashtbl.find in_out id in
      let def, use  =
        match Hashtbl.find_opt def_use id with
        | None   -> Reg_set.empty, Reg_set.empty
        | Some s -> s
      in
      let pred, succ = Hashtbl.find pred_succ id in
      let out = List.fold_left (fun acc s ->
        Reg_set.union acc (fst (Hashtbl.find in_out s)))
        Reg_set.empty succ
      in
      let in_ = Reg_set.union use (Reg_set.diff out def) in
      add id in_ out;
      if not (Reg_set.equal old_in in_) then
        List.iter (fun id -> Queue.add id file) pred;
      liveness_assgn ()
  in
  init (-1) rtl_fun.entry;
  liveness_assgn ();
  (* debug in_out *)
  let ppf = Debug.PrintRegAlloc.gen_ppf rtl_fun.name ".liveness" in
  Debug.PrintRegAlloc.print_liveness ppf in_out Reg_set.fold;
  Debug.PrintRegAlloc.close_ppf ppf;

  (* debug def_use *)
  let ppf = Debug.PrintRegAlloc.gen_ppf rtl_fun.name ".def_use" in
  Debug.PrintRegAlloc.print_liveness ppf def_use Reg_set.fold;
  Debug.PrintRegAlloc.close_ppf ppf;

  def_use, in_out, reg_nb_use

(* Interference Graph ------------------------------------------------------- *)

let interference_graph (f : pseudo_reg function_def) =
  let graph = Interference_graph.create () in

  let def_use, in_out, reg_nb_use = get_liveness f in

  Hashtbl.iter (fun id (defs, _) ->
    let out = match Hashtbl.find f.code id with
      | IMove (rd, w, _) ->
        Interference_graph.add rd w Interference_graph.Preference graph;
        Reg_set.remove w (snd (Hashtbl.find in_out id))
      | _ -> snd (Hashtbl.find in_out id)
    in
    let out = Reg_set.diff out defs in
    Reg_set.iter (fun def ->
      Reg_set.iter (fun o -> Interference_graph.add def o Interfere graph) out
    ) defs
  ) def_use;
  graph, reg_nb_use

(* Graph coloring ----------------------------------------------------------- *)

type color = Reg of Lang.Mips.register | Spill of int

let graph_coloring (f: pseudo_reg function_def) =
  let graph, reg_nb_use = interference_graph f in
  let color = Hashtbl.create 32 in

  let ppf = Debug.PrintRegAlloc.gen_ppf f.name ".alloc" in

  let nb_spill = ref (-1) in
  let new_spill =
    fun () -> incr nb_spill; !nb_spill
  in

  let get_color n =
    let p, c = List.fold_left (fun (p, acc) (id, e) ->
      match e, id with
      | Interference_graph.Preference, Real r -> (r :: p,  acc)
      | Interference_graph.Preference, _ -> (p, acc)
      | Interference_graph.Interfere, _  ->
        match id with
        | Real r -> p, List.filter (fun a -> a <> r) acc
        | _ ->
          match Hashtbl.find_opt color id with
          | None | Some (Spill _) -> (p, acc)
          | Some (Reg r) ->
            p, List.filter (fun a -> a <> r) acc
    ) ([], Regs.register) n in
    (* let p = List.filter (fun r -> List.mem r c) p in *)
    match p, c with
    | r :: _, _   -> Some r
    | _, []       -> None
    | _, r :: _   -> Some r
  in

  let open Debug.PrintRegAlloc in
  let rec simplify () =
    match Interference_graph.find_min_without_pref k graph with
    | Some (v, _) -> simplify_select ppf v; select v
    | None        -> coalesce ()

  and coalesce () =
    match Interference_graph.get_george_preference_edge k graph with
    | None -> freeze ()
    | Some (v1, v2) ->
      let v, rv = Interference_graph.merge v1 v2 graph in
      simplify_coalesce ppf v rv;
      let nbr = try Hashtbl.find reg_nb_use rv with _ -> 0 in
      let nbv = try Hashtbl.find reg_nb_use v  with _ -> 0 in
      Hashtbl.replace reg_nb_use v (nbr + nbv);
      simplify ();
      match v with
      | Real r -> Hashtbl.replace color rv (Reg r)
      | Pseu _ -> Hashtbl.replace color rv (Hashtbl.find color v)

  and freeze () =
    match Interference_graph.find_min k graph with
    | None        -> spill ()
    | Some (v, _) ->
      remove_pref ppf v;
      Interference_graph.remove_pref_edge v graph;
      simplify ()

  and spill () =
    if not (Interference_graph.is_empty graph) then
      match Interference_graph.get_min reg_nb_use graph with
      | None -> ()
      | Some r -> spill_reg ppf r; select r

  and select v =
    let nv = List.of_seq (Hashtbl.to_seq (Hashtbl.find graph v)) in
    Interference_graph.remove v graph;
    simplify ();
    match get_color nv with
    | Some c -> select_reg ppf v c; Hashtbl.replace color v (Reg c)
    | None   -> spilled_reg ppf v; Hashtbl.replace color v (Spill (new_spill ()))
  in

  simplify ();
  Debug.PrintRegAlloc.close_ppf ppf;
  color, !nb_spill + 1

