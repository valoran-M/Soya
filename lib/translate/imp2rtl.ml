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

(* Traduction ----------------------------------------------------------------*)

  let rec tr_expression exp reg dest =
    let id_node = new_node () in
    let node, entry = match exp with
    | Cst  n -> IOp ((OConst n),                    [], reg, dest), id_node
    | Bool b -> IOp ((OConst (if b then 1 else 0)), [], reg, dest), id_node
    | Var  v ->
      (match Hashtbl.find_opt env v with
      | Some rv -> IMove (reg, rv, dest), id_node
      | None -> ILoad (Addr v, reg, dest), id_node)
    | Binop (op, e1, e2) -> tr_binop op e1 e2 reg dest
    | Call (s, le) ->
      let id_call = new_node () in
      let args, entry =
        List.fold_right (fun exp (lr, dest) -> 
          let reg = new_reg () in
          let id_node = tr_expression exp reg dest in
          (reg :: lr, id_node)) le ([], id_call)
      in
      ICall (s, args, dest), entry;
    in
    Hashtbl.add code id_node node;
    entry 
  and tr_binop (op : binop) e1 e2 reg dest =
    let op =
      match op with
      | Add -> OAdd | Mul -> OMul
      | Lt  -> OLt
    in
    let r2 = new_reg () in
    let id_node = new_node () in
    let n2 = tr_expression e2 r2 id_node in
    let n1 = tr_expression e1 dest n2 in
    IOp (op, [dest; r2], reg, dest), n1
  in

  let tr_condition (cond : Lang.Imp.expression) destT destF =
    let id_cond = new_node () in
    match cond with
    | Binop (Lt, e1, e2) ->
      let r1 = new_reg () in
      let r2 = new_reg () in
      let id2 = tr_expression e2 r2 id_cond in
      let id1 = tr_expression e1 r1 id2 in
      ICond (CLt, [r1; r2], destT, destF), id1
    | _ ->
      let r = new_reg () in
      let id = tr_expression cond r id_cond in
      ICond (CEqi 1, [r], destT, destF), id
  in

  let rec tr_instruction (inst : Lang.Imp.instruction) dest =
    let id_node = new_node () in
    let node, entry =
      match inst with
      | Putchar e ->
        let reg = new_reg () in
        let n   = tr_expression e reg id_node in
        IPutchar reg, n
      | Set (id, e) ->
          (match Hashtbl.find_opt env id with
          | Some reg -> INop, tr_expression e reg id_node
          | None ->
            let reg = new_reg () in 
            IStore (Addr id, reg, id_node), tr_expression e reg id_node)
      | If (c, e1, e2) ->
        let node_T = tr_sequence e1 dest in
        let node_F = tr_sequence e2 dest in
        tr_condition c node_T node_F
      | While (c, e) ->
        let goto_id = new_node () in
        let node_T  = tr_sequence e id_node in
        let cond, cond_id = tr_condition c node_T dest in
        Hashtbl.add code cond_id cond;
        IGoto cond_id, goto_id
      | Return e ->
        let reg = new_reg () in
        let n   = tr_expression e reg id_node in
        IReturn reg, n
      | Expr e ->
        let reg = new_reg () in
        INop, tr_expression e reg dest
    in
    Hashtbl.add code id_node node;
    entry
  and tr_sequence seq entry =
    List.fold_right (fun inst entry ->
      tr_instruction inst entry
    ) seq entry
  in

  let entry = tr_sequence fdef.code (new_node ()) in
  {
    name = fdef.name;
    params = params;
    code = code;
    entry = entry;
  }

let tr_program (prog : Lang.Imp.program) =
  {
    globals = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }
