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
        incr_reg r;
        add_succ id n;
        add_def_use id [] [r];
        init id n
      | IMove (rd, r, n) ->
        incr_reg r; incr_reg rd;
        add_succ id n;
        add_def_use id [rd] [r];
        init id n
      | IOp (_, args, rd, n) ->
        incr_reg rd; List.iter incr_reg args;
        add_succ id n;
        add_def_use id [rd] (args);
        init id n
      | IPop  (rd, n)
      | ILoad (_, rd, n) ->
        incr_reg rd;
        add_succ id n;
        add_def_use id [rd] [];
        init id n
      | IPush (r, n)
      | IStore (_, r, n) ->
        incr_reg r;
        add_succ id n;
        add_def_use id [] [r];
        init id n
      | ICall (_, args, _, n) ->
        List.iter incr_reg args;
        add_succ id n;
        add_def_use id Regs.caller_saved (fun_args_to_reg args);
        init id n
      | ICond (_, args, nt, nf) ->
        List.iter incr_reg args;
        add_succ id nt; add_succ id nf;
        add_def_use id [] args;
        init id nt;
        init id nf
      | IReturn (Some r) ->
        incr_reg r;
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

let graph_coloring (f: pseudo_reg function_def) =
  let graph, reg_nb_use = interference_graph f in
  let color = Hashtbl.create 32 in

  let get_color n =
    let l = Seq.fold_left (fun acc (id, e) ->
      match e with
      | Interference_graph.Preference -> acc
      | Interference_graph.Interfere  ->
        match Hashtbl.find_opt color id with
        | None | Some (Pseu _) -> acc
        | Some (Real r) -> List.filter (fun a -> a = r) acc
    ) Regs.register n in
    match l with
    | []     -> None
    | r :: _ -> Some r
  in

  let rec simplify () =
    match Interference_graph.find_min_without_pref k graph with
    | Some (v, _) -> select v
    | None        -> coalesce ()

  and coalesce () =
    match Interference_graph.get_george_preference_edge k graph with
    | Some (v1, v2) ->
      let v, rv = Interference_graph.merge v1 v2 graph in
      simplify ();
      Hashtbl.replace color rv (Hashtbl.find color v)
    | None -> freeze ()

  and freeze () =
    match Interference_graph.find_min k graph with
    | Some (v, _) -> Interference_graph.remove_pref_edge v graph; simplify ()
    | None        -> spill ()

  and spill () =
    if not (Interference_graph.is_empty graph) then
      let min, _ = Hashtbl.fold (fun r nb (min, nb_min) ->
          if nb < nb_min || min = Real "" then r, nb else min, nb_min)
        reg_nb_use (Real "", 0) in
      select min

  and select v =
    let nv = Hashtbl.to_seq (Hashtbl.find graph v) in
    Interference_graph.remove v graph;
    simplify ();
    match get_color nv with
    | Some c -> Hashtbl.replace color v (Real c)
    | None   -> ()
  in

  simplify ();

  let print_pseudo_reg reg =
    match reg with
    | Pseu r -> Printf.sprintf "x%d" r
    | Real s -> Printf.sprintf "%s" s
  in

  Hashtbl.iter (fun id c -> 
    Printf.printf "%s -> %s\n" (print_pseudo_reg id) (print_pseudo_reg c)
  ) color

