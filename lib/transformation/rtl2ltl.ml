open Lang
open Lang.Ltl

type reg = Reg of Lang.Mips.register | Spill

let tr_function (def : Lang.Rtl.pseudo_reg Lang.Rtl.function_def) =
  let code = Hashtbl.create 16 in
  let color = Regalloc.graph_coloring def in
  
  let id_env = Hashtbl.create 32 in

  (* Pseudo register and node interface *)
  let new_node =
    let node = ref (-1) in
    (fun () -> incr node; !node)
  in

  let push_node l_id node =
    let id = new_node () in
    Hashtbl.replace id_env l_id id;
    Hashtbl.replace code id node;
    id
  in

  let get_reg r =
    match r with
    | Rtl.Real r -> Reg r
    | _ ->
      match Hashtbl.find color r with
      | Regalloc.Spill _ -> failwith "TODO\n" 
      | Regalloc.Reg r -> Reg r
  in

  let rec tr_instruction i =
    if Hashtbl.mem def.code i then
      let inst = Hashtbl.find def.code i in
      Hashtbl.remove def.code i;
      match inst with
      | Rtl.INop n   -> ignore (tr_instruction n); push_node i (INop n)
      | Rtl.IGoto n  -> ignore (tr_instruction n); push_node i (IGoto n)
      | Rtl.IPutchar (r, n) ->
        let dest = tr_instruction n in
        (match get_reg r with
        | Reg r -> push_node i (IPutchar (r, dest))
        | Spill -> failwith "TODO\n")
      | Rtl.IMove (r1, r2, n)  -> tr_move i r1 r2 (tr_instruction n)
      | Rtl.IOp (op, lr, r, n) -> tr_op i op lr r n
      | Rtl.ILoad  (_a, _r, _n) -> assert false
      | Rtl.IStore (_a, _r, _n) -> assert false
      | Rtl.IPush (_r, _n) -> assert false
      | Rtl.IPop (_r, _n) -> assert false
      | Rtl.ICall (id, _, i, n) -> push_node i (ICall (id, i, n))
      | Rtl.ICond (_c, _lr, _nt, _nf) -> assert false
      | Rtl.IReturn None -> push_node i (IReturn None)
      | _ -> assert false
    else 1
  and tr_move i r1 r2 dest =
    match get_reg r1, get_reg r2 with
    | Reg r1, Reg r2 ->
      if r1 = r2
      then dest
      else push_node i (IMove (r1, r2, dest))
    | Spill, Reg r2 -> push_node i (IPush (r2, dest))
    | _, _ -> assert false
  and tr_op _i _op _lr _r _dest =
    assert false
  and _tr_cond _c _lr _nt _nf =
    assert false
  in


  let entry = tr_instruction def.entry in
  Hashtbl.iter (fun id inst -> 
    match inst with
    | INop  n -> Hashtbl.replace code id (INop  (Hashtbl.find id_env n))
    | IGoto n -> Hashtbl.replace code id (IGoto (Hashtbl.find id_env n))
    | _ -> ()
  ) code;
  {
    name = def.name;
    code;
    entry
  }

let tr_program (prog : Rtl.pseudo_reg Rtl.program) : program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

