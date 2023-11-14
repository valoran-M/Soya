open Lang
open Lang.Ltl
open Lang.Op

type reg = Reg of Lang.Mips.register | Spill of int

let tr_function (def : Lang.Rtl.pseudo_reg Lang.Rtl.function_def) =
  let code = Hashtbl.create 16 in
  let color, nb_spilled = Regalloc.graph_coloring def in
  let stack_size = nb_spilled * 4 + def.max_pushed_args * 4 in
  
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

  let spill_addr n =
    AddrStack (-4 * n - 4 * def.max_pushed_args)
  in

  let get_reg r =
    match r with
    | Rtl.Real r -> Reg r
    | _ ->
      match Hashtbl.find color r with
      | Regalloc.Spill n -> Spill n
      | Regalloc.Reg r   -> Reg r
  in

  let tr_addr = function
    | Lang.Op.Addr i -> Lang.Op.Addr i, None
    | Lang.Op.AddrStack i -> Lang.Op.AddrStack i, None
    | Lang.Op.AddrGlobl i -> Lang.Op.AddrGlobl i, None
    | Lang.Op.AddrOReg (i, r) ->(
      match get_reg r with
      | Reg r -> Lang.Op.AddrOReg (i, r), None
      | Spill n -> Lang.Op.AddrOReg (i, Mips.t8), Some n)
    | Lang.Op.AddrReg r ->
      match get_reg r with
      | Reg r -> Lang.Op.AddrReg r, None
      | Spill n -> Lang.Op.AddrReg Mips.t8, Some n
  in

  let rec tr_instruction i =
    match Hashtbl.find_opt id_env i with
    | Some i' -> i'
    | None ->
      let nid = new_node () in
      Hashtbl.add id_env i nid;
      let bid = match Hashtbl.find def.code i with
        | INop n             -> push_node (IGoto ((tr_instruction n)))
        | IPutchar (r,n)     -> tr_putchar r (tr_instruction n)
        | IAlloc (r,_,n)     -> tr_alloc r (tr_instruction n)
        | IGoto n            -> push_node (IGoto (tr_instruction n))
        | IReturn None       -> push_node (IReturn None)
        | IMove (r1,r2,n)    -> tr_move r1 r2 (tr_instruction n)
        | IOp (op,lr,r,n)    -> tr_op op lr r (tr_instruction n)
        | ILoad  (a,r,s,n)   -> tr_load a r s (tr_instruction n)
        | IStore (a,r,s,n)   -> tr_store a r s (tr_instruction n)
        | IGetParam (r,i,a,n)-> tr_get_param r i a (tr_instruction n)
        | ISetParam (r,i,a,n)-> tr_set_param r i a (tr_instruction n)
        | ICall (a,_,i,_,n)  -> tr_call a i (tr_instruction n)
        | ICond (c,lr,nt,nf) -> tr_cond c lr (tr_instruction nt)
                                             (tr_instruction nf)
        | IReturn _ -> assert false
      in
      let node = Hashtbl.find code bid in
      Hashtbl.remove code bid;
      Hashtbl.replace id_env i nid;
      Hashtbl.replace code nid node;
      nid
  and tr_call a i dest =
    let addr, spilled = tr_addr a in
    match spilled with
    | None    -> push_node (ICall (addr, i, dest))
    | Some n  -> push_node (ILoad (spill_addr n, Mips.t8, Word,
                 push_node (ICall (addr, i, dest))))
  and tr_load a r s dest =
    let addr, spilled = tr_addr a in
    let dest =
      match get_reg r with
      | Reg r   -> push_node (ILoad (addr, r, s, dest))
      | Spill n -> push_node (ILoad (spill_addr n, Mips.t8, s, push_node
                             (ILoad (addr, Mips.t8, s, dest))))
    in
    match spilled with
    | None -> dest
    | Some n -> push_node (ILoad (spill_addr n, Mips.t8, s, dest))

  and tr_store a r s dest =
    let addr, spilled = tr_addr a in
    let dest =
      match get_reg r with
      | Reg r   -> push_node (IStore (addr, r, s, dest))
      | Spill n -> push_node (ILoad (spill_addr n, Mips.t8, s, push_node
                             (IStore (addr, Mips.t8, s, dest))))
    in
    match spilled with
    | None -> dest
    | Some n -> push_node (ILoad (spill_addr n, Mips.t8, s, dest))

  and tr_get_param regs i nb_pushed dest =
    let addr = AddrStack ((i + 1 - nb_pushed) * 4 + stack_size) in
    match get_reg regs with
    | Reg r   -> push_node (ILoad (addr, r, Word, dest))
    | Spill n -> push_node (ILoad (addr, Mips.t8, Word, push_node
                           (IStore (spill_addr n, Mips.t8, Word, dest))))

  and tr_set_param regs i nb_pushed dest =
    let addr = AddrStack ((i + 1 - nb_pushed) * 4) in
    match get_reg regs with
    | Reg r   -> push_node (IStore (addr, r, Word, dest))
    | Spill n -> push_node (ILoad (addr, Mips.t8, Word, push_node
                           (IStore (spill_addr n, Mips.t8, Word, dest))))

  and tr_putchar r dest =
    match get_reg r with
    | Reg r   -> push_node (IPutchar (r, dest))
    | Spill n -> push_node (ILoad (spill_addr n, Mips.t8, Word, push_node
                           (IPutchar (Mips.t8, dest))))

  and tr_alloc r dest =
    match get_reg r with
    | Reg r   -> push_node (IAlloc (r, dest))
    | Spill n -> push_node (ILoad (spill_addr n, Mips.t8, Word, push_node
                           (IAlloc (Mips.t8, dest))))
  and tr_move r1 r2 dest =
    match get_reg r1, get_reg r2 with
    | Reg r1, Reg r2 ->
      if r1 = r2 then push_node (IGoto dest)
                 else push_node (IMove (r1, r2, dest))
    | Spill n, Reg r2  ->
      push_node (IStore (spill_addr n, r2, Word, dest))
    | Reg r,   Spill n ->
      push_node (ILoad (spill_addr n, r, Word, dest))
    | Spill n1, Spill n2 ->
      if n1 = n2 then dest
      else push_node (ILoad (spill_addr n2, Mips.t8, Word, push_node
                     (IStore (spill_addr n1, Mips.t8, Word, dest))))
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
      | Spill n -> push_node (ILoad (spill_addr n, r, Word, dest)))
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
                             (IStore (spill_addr n, Mips.t8, Word, dest)))))
    in
    List.fold_left2 (fun dest pr r ->
      match get_reg pr with
      | Reg _   -> dest
      | Spill n -> push_node (ILoad (spill_addr n, r, Word, dest)))
    dest args regs
  in

  let entry = tr_instruction def.entry in
  {
    stack_size;
    name = def.name;
    code;
    entry;
  }

let tr_program (prog : Rtl.pseudo_reg Rtl.program) : program =
  {
    globals   = prog.globals;
    static    = prog.static;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

