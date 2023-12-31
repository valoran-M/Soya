open Lang.Rtl
open Lang.Imp
(* open Lang.Op *)

let tr_function (fdef : Lang.Imp.function_def) =
  (* RTL code *)
  let code = Hashtbl.create 16 in
  let env  = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let reg  = ref (-1) in
  let new_reg, new_node =
    let node = ref (-1) in
    (fun () -> incr reg;  Pseudo !reg ), (fun () -> incr node; !node)
  in

  (* Save params and local variable in environement *)
  let params =
    List.map (fun a ->
      let r = new_reg () in
      Hashtbl.replace env a r; r)
      fdef.params
  in
  List.iter (fun a -> Hashtbl.replace env a (new_reg ())) fdef.locals;

  let push_node node =
    let id = new_node () in
    Hashtbl.replace code id node;
    id
  in

  (* Translate ---------------------------------------------------------------*)

  let rec tr_expression exp (reg : pseudo option) dest =
    match exp, reg with
    | Char c, Some reg -> push_node (IOp ((OChar c), [], reg, dest))
    | Cst  n, Some reg -> push_node (IOp ((OConst n), [], reg, dest))
    | Bool b, Some reg -> push_node (IOp ((OConst (if b then 1 else 0)), [], reg, dest))
    | Addr l, Some reg -> push_node (IOp (OLabel l, [], reg, dest))
    | Deref (e, s), Some reg -> tr_deref e s reg dest
    | Alloc e, Some reg ->
      let r = new_reg () in
      tr_expression e (Some r) (push_node (IAlloc (r, Some reg, dest)))
    | Var  v, Some reg ->
      (match Hashtbl.find_opt env v with
      | Some rv -> if reg <> rv then push_node (IMove (reg, rv, dest)) else dest
      | None    -> push_node (ILoad (AddrGlobl v, reg, Word, dest)))
    | Unop (op, e), _       -> tr_unop  op e reg dest
    | Binop (op, e1, e2), _ -> tr_binop op e1 e2 reg dest
    | DCall (e, le), _ ->
      let id_call = new_node () in
      let freg = new_reg () in
      let id_f = tr_expression e (Some freg) id_call in
      let args, entry, _ =
        List.fold_right (fun exp (lr, dest, nb_arg) -> 
          let reg =  new_reg () in
          let id_node = tr_expression exp (Some reg) dest in
          (reg :: lr, id_node, nb_arg - 1))
        le ([], id_f, List.length le - 1)
      in
      Hashtbl.replace code id_call
        (ICall (AddrReg freg, args, List.length args, reg, dest));
      entry
    | Call (s, le), _ ->
      let id_call = new_node () in
      let args, entry, _ =
        List.fold_right (fun exp (lr, dest, nb_arg) -> 
          let reg =  new_reg () in
          let id_node = tr_expression exp (Some reg) dest in
          (reg :: lr, id_node, nb_arg - 1))
        le ([], id_call, List.length le - 1)
      in
      Hashtbl.replace code id_call
        (ICall (Addr s, args, List.length args, reg, dest));
      entry
    | _, None -> dest
  
  and tr_deref (e : expression) s reg dest =
    let r = new_reg () in
    match e with
    | Binop (Add, Cst c, e) | Binop (Add, e, Cst c) ->
      tr_expression e (Some r)
        (push_node (ILoad ((AddrOReg (c, r)), reg, s, dest)))
    | _ ->
      tr_expression e (Some r) (push_node (ILoad ((AddrReg r), reg, s, dest)))

  and tr_unop (op : unop) e reg dest =
    match reg with
    | None -> tr_expression e None dest
    | Some reg ->
      let op =
        match op with
        | Neg -> Lang.Op.ONeg
        | Not -> Lang.Op.ONot
      in
      let r = new_reg () in
      let id_op = push_node (IOp (op, [r], reg, dest)) in
      tr_expression e (Some r) id_op

  and tr_binop (op : binop) e1 e2 reg dest =
    match reg with
    | None -> tr_expression e1 None (tr_expression e2 None dest)
    | Some reg ->
      let op =
        match op with
        | Sub -> Lang.Op.OSub
        | Add -> Lang.Op.OAdd | Mul -> Lang.Op.OMul
        | Lt  -> Lang.Op.OLt  | Le  -> Lang.Op.OLe
        | Gt  -> Lang.Op.OGt  | Ge  -> Lang.Op.OGe
        | Eq  -> Lang.Op.OEq  | Neq -> Lang.Op.ONeq
        | And -> Lang.Op.OAnd | Or  -> Lang.Op.OOr
      in
      let r1 = new_reg () in
      let r2 = new_reg () in
      let id_op = push_node (IOp (op, [r1; r2], reg, dest)) in
      let n2 = tr_expression e2 (Some r2) id_op in
      tr_expression e1 (Some r1) n2
  in

  let rec tr_condition (cond : Lang.Imp.expression) destT destF =
    match cond with
    | Binop ((Lt | Le | Gt | Ge | Eq | Neq as op), e1, e2) ->
      let op =
        match op with
        | Lt  -> Lang.Op.CLt
        | Le  -> Lang.Op.CLe
        | Gt  -> Lang.Op.CGt
        | Ge  -> Lang.Op.CGe
        | Eq  -> Lang.Op.CEq
        | Neq -> Lang.Op.CNeq
        | _ -> assert false
      in
      let r1 = new_reg () in
      let r2 = new_reg () in
      let id_cond = push_node (ICond(op, [r1; r2], destT, destF)) in
      let id2 = tr_expression e2 (Some r2) id_cond in
      tr_expression e1 (Some r1) id2
    | Binop (And, c1, c2) ->
      let cond_c2 = tr_condition c2 destT destF in
      tr_condition c1 cond_c2 destF
    | Binop (Or, c1, c2) ->
      let cond_c2 = tr_condition c2 destT destF in
      tr_condition c1 destT cond_c2 
    | Bool b ->
      let c = if b then 1 else 0 in
      push_node (ICond (CConst c, [ ], destT, destF))
    | _ ->
      let r = new_reg () in
      let id_node = push_node (ICond (CEqi 1, [r], destT, destF)) in
      tr_expression cond (Some r) id_node
  in

  let rec tr_instruction (inst : Lang.Imp.instruction) dest =
    match inst with
    | Putchar e ->
      let reg = new_reg () in
      let id_put = push_node (IPutchar (reg, dest)) in
      tr_expression e (Some reg) id_put
    | Set (id, e) ->
        (match Hashtbl.find_opt env id with
        | Some reg -> tr_expression e (Some reg) dest
        | None ->
          let reg = new_reg () in
          let id_store = push_node (IStore ((AddrGlobl id), reg, Word, dest)) in
          tr_expression e (Some reg) id_store)
    | If (c, e1, e2) ->
      let node_T = tr_sequence e1 dest in
      let node_F = tr_sequence e2 dest in
      tr_condition c node_T node_F
    | While (c, e) ->
      let id_goto = new_node () in
      let node_T  = tr_sequence e id_goto in
      let id_cond = tr_condition c node_T dest in
      Hashtbl.replace code id_goto (IGoto id_cond);
      id_cond
    | Return e ->
      let reg = new_reg () in
      tr_expression e (Some reg) (push_node (IReturn (Some reg)))
    | Expr e ->
      tr_expression e None dest
    | Write (d, s, e) ->
      let re = new_reg () in
      let rd = new_reg () in
      tr_expression e (Some re) (
      tr_expression d (Some rd) (
      push_node (IStore (AddrReg rd, re, s, dest))))

  and tr_sequence seq entry =
    List.fold_right (fun inst entry ->
      tr_instruction inst entry
    ) seq entry
  in

  let entry = tr_sequence fdef.code (push_node (IReturn None)) in
  {
    nb_reg = !reg;
    name = fdef.name;
    params;
    code;
    entry;
    max_pushed_args = 0;
  }

let tr_program (prog : Lang.Imp.program) : pseudo Lang.Rtl.program =
  {
    globals   = prog.globals;
    static    = prog.static;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

