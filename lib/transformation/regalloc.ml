open Lang.Rtl
open Lang.Mips

module Reg_set = Set.Make(String)

(* Liveness ----------------------------------------------------------------- *)

let register =
  [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9;
   s0; s1; s2; s3; s4; s5; s6; s7]

let caller_saved = [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9]
let callee_saved = [s0; s1; s2; s3; s4; s5; s6; s7]

let get_liveness (rtl_fun : function_def) =
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

  let reg = function 
    | Pseudo n -> Printf.sprintf "x%d" n
    | Real r   -> r
  in
  
  (* Init data (in/out and pred/succ) for get liveness *)
  let rec init pred id =
    if not (Hashtbl.mem def_use id) then (
      add_pred id pred;
      match Hashtbl.find rtl_fun.code id with
      | INop n -> add_succ id n; init id n
      | IPutchar (r, n) ->
        add_succ id n;
        add_def_use id [] [reg r];
        init id n
      | IMove (rd, r, n) ->
        add_succ id n;
        add_def_use id [reg rd] [reg r];
        init id n
      | IOp (_, args, rd, n) ->
        add_succ id n;
        add_def_use id [reg rd] (List.map reg args);
        init id n
      | ILoad (_, rd, n) ->
        add_succ id n;
        add_def_use id [reg rd] [];
        init id n
      | IStore (_, r, n) ->
        add_succ id n;
        add_def_use id [] [reg r];
        init id n
      | ICall (_, args, n) ->
        add_succ id n;
        add_def_use id caller_saved (Utils.fun_args_to_reg reg args);
        init id n
      | ICond (_, args, nt, nf) ->
        add_succ id nt; add_succ id nf;
        add_def_use id [] (List.map reg args);
        init id nt;
        init id nf
      | IReturn (Some _) ->
        add_def_use id [] (v0 :: callee_saved)
      | IReturn None -> ()
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
  Hashtbl.iter (fun id _ -> Printf.printf "%d\n" id; Queue.add id file) rtl_fun.code;
  
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
      Printf.printf "%d : " id;
      Reg_set.iter (fun reg -> Printf.printf "%s " reg) in_;
      Printf.printf " \ ";
      if not (Reg_set.equal old_in in_) then (
        List.iter (fun id -> Printf.printf "%d " id; Queue.add id file) pred);
      Printf.printf "\n";
      liveness_assgn ()
  in

  init (-1) rtl_fun.entry;
  liveness_assgn ();
  in_out

(* Reg Alloc ---------------------------------------------------------------- *)

