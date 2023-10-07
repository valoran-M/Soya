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

let fun_args_to_reg =
  let rec aux acc regs args =
    match regs, args with
    | _,         []        -> acc
    | [],        a :: args -> aux (a :: acc) [] args
    | r :: reg,  _ :: args -> aux (r :: acc) reg args
  in
  aux [] [Real a0; Real a1; Real a2; Real a3]

let k = Array.length Regs.register

(* Liveness ----------------------------------------------------------------- *)

let get_liveness (rtl_fun : pseudo_reg function_def) =
  (* Def and Use array *)
  let def_use = Hashtbl.create 32 in
  let add_def_use id def use =
    Hashtbl.replace def_use id (Reg_set.of_list def, Reg_set.of_list use)
  in

  (* pred and succ array *)
  let pred_succ = Hashtbl.create 32 in
  Hashtbl.iter (fun id _ -> Hashtbl.replace pred_succ id ([], [])) rtl_fun.code;
  let add_pred id pred =
    if pred >= 0 then
      let p, s = Hashtbl.find pred_succ id in
      Hashtbl.replace pred_succ id (pred :: p, s)
  in
  let add_succ id succ =
    match Hashtbl.find_opt pred_succ id with
    | None        -> Hashtbl.replace pred_succ id ([], [succ]);
    | Some (p, s) -> Hashtbl.replace pred_succ id (p,  succ :: s)
  in

  (* Init data (in/out and pred/succ) for get liveness *)
  let rec init pred id =
    add_pred id pred;
    if not (Hashtbl.mem def_use id) then (
      match Hashtbl.find rtl_fun.code id with
      | INop n -> add_succ id n; init id n
      | IPutchar (r, n) ->
        add_succ id n;
        add_def_use id [] [r];
        init id n
      | IMove (rd, r, n) ->
        add_succ id n;
        add_def_use id [rd] [r];
        init id n
      | IOp (_, args, rd, n) ->
        add_succ id n;
        add_def_use id [rd] (args);
        init id n
      | IPop  (rd, n)
      | ILoad (_, rd, n) ->
        add_succ id n;
        add_def_use id [rd] [];
        init id n
      | IPush (r, n)
      | IStore (_, r, n) ->
        add_succ id n;
        add_def_use id [] [r];
        init id n
      | ICall (_, args, _, n) ->
        add_succ id n;
        add_def_use id Regs.caller_saved (fun_args_to_reg args);
        init id n
      | ICond (_, args, nt, nf) ->
        add_succ id nt; add_succ id nf;
        add_def_use id [] args;
        init id nt;
        init id nf
      | IReturn (Some _) ->
        add_def_use id [] ((Real v0) :: Regs.callee_saved)
      | IReturn None ->
        add_def_use id [] Regs.callee_saved
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
  def_use, in_out

(* Interference Graph ------------------------------------------------------- *)

let interference_graph (f : pseudo_reg function_def) =
  let graph = Inter_graph.create () in

  let def_use, in_out = get_liveness f in

  Hashtbl.iter (fun id (defs, _) ->
    let out = match Hashtbl.find f.code id with
      | IMove (rd, w, _) ->
        Inter_graph.add rd w Inter_graph.Preference graph;
        Reg_set.remove w (snd (Hashtbl.find in_out id))
      | _ -> snd (Hashtbl.find in_out id)
    in
    let out = Reg_set.diff out defs in
    Reg_set.iter (fun def ->
      Reg_set.iter (fun o -> Inter_graph.add def o Interfere graph) out
    ) defs
  ) def_use;
  graph

(* Graph coloring ----------------------------------------------------------- *)

type color = Reg of int | Stack

let graph_coloring (f: pseudo_reg function_def) =
  let graph = interference_graph f in
  let _color = Hashtbl.create 32 in

  let rec simplify () =
    let simplify_find id _ acc =
      if Inter_graph.has_preference id graph then acc
      else
        let degree = Inter_graph.degree id graph in
        match acc with
        | Some (_, d) -> if degree < d then Some (id, degree) else acc
        | None        -> if degree < k then Some (id, degree) else acc
    in
    match Hashtbl.fold simplify_find graph None with
    | Some (v, _) -> select v
    | None        -> coalesce ()
  and coalesce () =
    ()
  and _freeze () =
    let simplify_find id _ acc =
      let degree = Inter_graph.degree id graph in
      match acc with
      | Some (_, d) -> if degree < d then Some (id, degree) else acc
      | None        -> if degree < k then Some (id, degree) else acc
    in
    match Hashtbl.fold simplify_find graph None with
    | Some (v, _) -> Inter_graph.remove_pref_edge v graph; simplify ()
    | None        -> _spill ()
  and _spill () =
    ()
  and select _v =
    ()
  in

  simplify ()

