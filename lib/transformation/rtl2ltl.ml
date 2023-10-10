open Lang
open Lang.Ltl

type reg = Reg of Lang.Mips.register | Spill of int

let tr_function (def : Lang.Rtl.pseudo_reg Lang.Rtl.function_def) =
  let code = Hashtbl.create 16 in
  let color, nb_spilled = Regalloc.graph_coloring def in
  
  let id_env = Hashtbl.create 32 in

  let new_node =
    let node = ref (-1) in
    (fun () -> incr node; !node)
  in

  let push_node node =
    let id = new_node () in
    Hashtbl.replace code id node;
    id
  in

  let get_reg r =
    match r with
    | Rtl.Real r -> Reg r
    | _ ->
      match Hashtbl.find color r with
      | Regalloc.Spill n -> Spill n
      | Regalloc.Reg r   -> Reg r
  in

  let rec tr_instruction i =
    match Hashtbl.find_opt id_env i with
    | Some i' -> i'
    | None ->
      let nid = new_node () in
      Hashtbl.add id_env i nid;
      let bid = match Hashtbl.find def.code i with
        | INop n             -> push_node (IGoto ((tr_instruction n)))
        | IGoto n            -> push_node (IGoto (tr_instruction n))
        | IReturn None       -> push_node (IReturn None)
        | IPutchar (r,n)     -> tr_putchar r (tr_instruction n)
        | IMove (r1,r2,n)    -> tr_move r1 r2 (tr_instruction n)
        | IOp (op,lr,r,n)    -> tr_op op lr r (tr_instruction n)
        | ILoad  (a,r,n)     -> tr_load a r (tr_instruction n)
        | IStore (a,r,n)     -> tr_store a r (tr_instruction n)
        | IPush (r,n)        -> tr_push r (tr_instruction n)
        | ICall (id,_,i,_,n) -> push_node (ICall (id, i, (tr_instruction n)))
        | ICond (c,lr,nt,nf) -> tr_cond c lr (tr_instruction nt)
                                             (tr_instruction nf)
        | _ -> assert false
      in
      let node = Hashtbl.find code bid in
      Hashtbl.remove code bid;
      Hashtbl.replace id_env i nid;
      Hashtbl.replace code nid node;
      nid
  and tr_load a r dest =
    (match get_reg r with
    | Reg r   -> push_node (ILoad (a, r, dest))
    | Spill n -> push_node (ILoad (a, Mips.t8, push_node
                           (IStore (Rtl.AddrStack (-4 * n), Mips.t8, dest)))))
  and tr_store a r dest =
    (match get_reg r with
    | Reg r   -> push_node (IStore (a, r, dest))
    | Spill n -> push_node (ILoad (Rtl.AddrStack (-4 * n), Mips.t8, push_node
                           (IStore (a, Mips.t8, dest)))))
  and tr_push r dest =
    (match get_reg r with
    | Reg r   -> push_node (IPush (r, dest))
    | Spill n -> push_node (ILoad (Rtl.AddrStack (-4 * n), Mips.t8, push_node
                           (IPush (Mips.t8, dest)))))
  and tr_putchar r dest=
    (match get_reg r with
    | Reg r   -> push_node (IPutchar (r, dest))
    | Spill n -> push_node (ILoad (Rtl.AddrStack (-4 * n), Mips.t8, push_node
                           (IPutchar (Mips.t8, dest)))))
  and tr_move r1 r2 dest =
    match get_reg r1, get_reg r2 with
    | Reg r1, Reg r2 ->
      if r1 = r2 then dest
                 else push_node (IMove (r1, r2, dest))
    | Spill n, Reg r2  ->
      push_node (IStore (Rtl.AddrStack (-4 * n), r2, dest))
    | Reg r,   Spill n ->
      push_node (IMove (r, Mips.t8, push_node
                (ILoad (Rtl.AddrStack (-4 * n), Mips.t8, dest))))
    | Spill n1, Spill n2 ->
      push_node (IStore (Rtl.AddrStack (-4 * n1), Mips.t8, push_node
                (ILoad (Rtl.AddrStack (-4 * n2), Mips.t8, dest))))
  and tr_cond c args nt nf =
    let regs = fst (List.fold_right (fun r (a, t) -> 
      match get_reg r with
      | Reg r   -> (r :: a, t)
      | Spill _ -> match t with
                  | r :: t -> (r :: a, t)
                  | _ -> assert false
    ) args ([], [Mips.t8; Mips.t9])) in
    let dest = push_node (ICond (c, regs, nt, nf)) in
    List.fold_left2 (fun dest pr r ->
      match get_reg pr with
      | Reg _   -> dest
      | Spill n -> push_node (IStore (Rtl.AddrStack (-4 * n), r, dest)))
    dest args regs
  and tr_op op args r dest =
    let regs = fst (List.fold_right (fun r (a, t) -> 
      match get_reg r with
      | Reg r   -> (r :: a, t)
      | Spill _ -> match t with
                  | r :: t -> (r :: a, t)
                  | _ -> assert false
    ) args ([], [Mips.t8; Mips.t9])) in
    let dest = match get_reg r with
      | Reg r   -> push_node (IOp (op, regs, r, dest))
      | Spill n -> push_node (IOp (op, regs, Mips.t8, (push_node
                             (IStore (Rtl.AddrStack (-4 * n), Mips.t8, dest)))))
    in
    List.fold_left2 (fun dest pr r ->
      match get_reg pr with
      | Reg _   -> dest
      | Spill n -> push_node (IStore (Rtl.AddrStack (-4 * n), r, dest)))
    dest args regs
  in

  let entry = tr_instruction def.entry in
  {
    stack_size = nb_spilled * 4;
    name = def.name;
    code;
    entry
  }

let tr_program (prog : Rtl.pseudo_reg Rtl.program) : program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

