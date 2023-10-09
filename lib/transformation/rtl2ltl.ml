open Lang
open Lang.Ltl

type reg = Reg of Lang.Mips.register | Spill of int

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
      | Regalloc.Reg r   -> Reg r
  in

  let rec tr_instruction i =
    if Hashtbl.mem def.code i then
      let inst = Hashtbl.find def.code i in
      Hashtbl.remove def.code i;
      match inst with
      | Rtl.INop n                -> push_node i (IGoto ((tr_instruction n)))
      | Rtl.IGoto n               -> push_node i (IGoto (tr_instruction n))
      | Rtl.ICall (id, _, i, n)   -> push_node i (ICall (id, i, n))
      | Rtl.IReturn None          -> push_node i (IReturn None)
      | Rtl.IPutchar (r, n)       -> tr_putchar i r (tr_instruction n)
      | Rtl.IMove (r1, r2, n)     -> tr_move i r1 r2 (tr_instruction n)
      | Rtl.IOp (op, lr, r, n)    -> tr_op i op lr r n
      | Rtl.ILoad  (a, r, n)      -> tr_load i a r (tr_instruction n)
      | Rtl.IStore (a, r, n)      -> tr_store i a r (tr_instruction n)
      | Rtl.IPush (r, n)          -> tr_push i r (tr_instruction n)
      | Rtl.IPop (r, n)           -> tr_pop i r (tr_instruction n)
      | Rtl.ICond (c, lr, nt, nf) ->
        tr_cond c lr (tr_instruction nt) (tr_instruction nf)
      | _ -> assert false
    else 1
  and tr_load i a r dest =
    (match get_reg r with
    | Reg r   -> push_node i (ILoad (a, r, dest))
    | Spill n -> push_node i (ILoad (a, Mips.t8, push_node i
                             (IStore (Rtl.AddrStack n, Mips.t8, dest)))))
  and tr_store i a r dest =
    (match get_reg r with
    | Reg r   -> push_node i (IStore (a, r, dest))
    | Spill n -> push_node i (ILoad (Rtl.AddrStack n, Mips.t8, push_node i
                             (IStore (a, Mips.t8, dest)))))
  and tr_push i r dest =
    (match get_reg r with
    | Reg r   -> push_node i (IPush (r, dest))
    | Spill n -> push_node i (ILoad (Rtl.AddrStack n, Mips.t8, push_node i
                             (IPush (Mips.t8, dest)))))
  and tr_pop i r dest =
    (match get_reg r with
    | Reg r   -> push_node i (IPop (r, dest))
    | Spill n -> push_node i (IPop (Mips.t8, push_node i
                             (IStore (Rtl.AddrStack n, Mips.t8, dest)))))
  and tr_putchar i r dest=
    (match get_reg r with
    | Reg r -> push_node i (IPutchar (r, dest))
    | Spill n -> push_node i (ILoad (Rtl.AddrStack n, Mips.t8, push_node i
                             (IPutchar (Mips.t8, dest)))))
  and tr_move i r1 r2 dest =
    match get_reg r1, get_reg r2 with
    | Reg r1, Reg r2 ->
      if r1 = r2 then dest
                 else push_node i (IMove (r1, r2, dest))
    | Spill n, Reg r2  ->
      push_node i (IStore (Rtl.AddrStack n, r2, push_node i
                  (IPush (r2, dest))))
    | Reg r,   Spill n ->
      push_node i (IMove (r, Mips.t8, push_node i
                  (ILoad (Rtl.AddrStack n, Mips.t8, dest))))
    | Spill n1, Spill n2 ->
      push_node i (IStore (Rtl.AddrStack n1, Mips.t8, push_node i
                  (ILoad (Rtl.AddrStack n2, Mips.t8, dest))))
  and tr_cond _c _lr _nt _nf =
    assert false
  and tr_op _i _op _lr _r _dest =
    assert false
  in

  let entry = tr_instruction def.entry in
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

