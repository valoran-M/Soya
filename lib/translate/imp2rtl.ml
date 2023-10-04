open Lang.Imp
open Lang.Rtl

let tr_function (fdef : Lang.Imp.function_def) =
  (* RTL code *)
  let code = Hashtbl.create 16 in
  let env  = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let new_reg, new_node =
    let reg  = ref (-1) in
    let node = ref (-1) in
    (fun () -> incr reg;  !reg ), (fun () -> incr node; !node)
  in

  (* Save params and local variable in environement *)
  let params =
    List.map (fun a ->
      let r = new_reg () in
      Hashtbl.add env a r; r)
      fdef.params
  in
  List.iter (fun a -> Hashtbl.add env a (new_reg ())) fdef.locals;

  let push_node node =
    let id = new_node () in
    Hashtbl.add code id node;
    id
  in

  (* Translate ---------------------------------------------------------------*)

  let rec tr_expression exp reg dest =
    match exp with
    | Cst  n -> push_node (IOp ((OConst n), [], reg, dest))
    | Bool b -> push_node (IOp ((OConst (if b then 1 else 0)), [], reg, dest))
    | Var  v ->
      (match Hashtbl.find_opt env v with
      | Some rv -> push_node (IMove (reg, rv, dest))
      | None    -> push_node (ILoad (AddrGlobl v, reg, dest)))
    | Binop (op, e1, e2) -> tr_binop op e1 e2 reg dest
    | Call (s, le) ->
      let id_call = new_node () in
      let args, entry =
        List.fold_right (fun exp (lr, dest) -> 
          let reg = new_reg () in
          let id_node = tr_expression exp reg dest in
          (reg :: lr, id_node)) le ([], id_call)
      in
      Hashtbl.add code id_call (ICall (s, args, dest));
      entry
  and tr_binop (op : binop) e1 e2 reg dest =
    let op =
      match op with
      | Add -> OAdd | Mul -> OMul
      | Lt  -> OLt
    in
    let r2 = new_reg () in
    let id_op = push_node (IOp (op, [dest; r2], reg, dest)) in
    let n2 = tr_expression e2 r2 id_op in
    tr_expression e1 dest n2
  in

  let tr_condition (cond : Lang.Imp.expression) destT destF =
    match cond with
    | Binop (Lt, e1, e2) ->
      let r1 = new_reg () in
      let r2 = new_reg () in
      let id_cond = push_node (ICond(CLt, [r1; r2], destT, destF)) in
      let id2 = tr_expression e2 r2 id_cond in
      tr_expression e1 r1 id2
    | _ ->
      let r = new_reg () in
      let id_node = push_node (ICond (CEqi 1, [r], destT, destF)) in
      tr_expression cond r id_node
  in

  let rec tr_instruction (inst : Lang.Imp.instruction) dest =
    match inst with
    | Putchar e ->
      let reg = new_reg () in
      let id_put = push_node (IPutchar (reg, dest)) in
      tr_expression e reg id_put
    | Set (id, e) ->
        (match Hashtbl.find_opt env id with
        | Some reg -> tr_expression e reg dest
        | None ->
          let reg = new_reg () in
          let id_store = push_node (IStore ((AddrGlobl id), reg, dest)) in
          tr_expression e reg id_store)
    | If (c, e1, e2) ->
      let node_T = tr_sequence e1 dest in
      let node_F = tr_sequence e2 dest in
      tr_condition c node_T node_F
    | While (c, e) ->
      let id_goto = new_node () in
      let node_T  = tr_sequence e id_goto in
      let id_cond = tr_condition c node_T dest in
      Hashtbl.add code id_goto (IGoto id_cond);
      id_cond
    | Return e ->
      let reg = new_reg () in
      tr_expression e reg (push_node (IReturn (Some reg)))
    | Expr e ->
      let reg = new_reg () in
      tr_expression e reg dest
  and tr_sequence seq entry =
    List.fold_right (fun inst entry ->
      tr_instruction inst entry
    ) seq entry
  in

  let entry = tr_sequence fdef.code (push_node (IReturn None)) in
  {
    name = fdef.name;
    params;
    code;
    entry;
  }

let tr_program (prog : Lang.Imp.program) =
  {
    globals = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

