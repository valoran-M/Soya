open Lang.Imp
open Lang.Rtl

let tr_function (fdef : Lang.Imp.function_def) =
  (* RTL code *)
  let code = Hashtbl.create 16 in
  let env  = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let new_reg, res_reg, new_node, res_node =
    let reg = ref (-1) in
    let node = ref (-1) in
    (fun () -> incr reg;  !reg ), (fun () -> reg := 0 ),
    (fun () -> incr node; !node), (fun () -> node := 0)
  in
  res_reg  ();
  res_node ();

  let rec tr_expression exp reg dest =
    let id_node = new_node () in
    let node, entry = match exp with
    | Cst  n -> IOp ((OConst n),                    [], reg, dest), id_node
    | Bool b -> IOp ((OConst (if b then 1 else 0)), [], reg, dest), id_node
    | Var _v -> assert false
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
      | Lt -> OLt
    in
    let r2 = new_reg () in
    let id_node = new_node () in
    let n2 = tr_expression e2 r2 id_node in
    let n1 = tr_expression e1 dest n2 in
    IOp (op, [dest; r2], reg, dest), n1
  in

  let tr_instruction (inst : Lang.Imp.instruction) dest =
    let id_node = new_node () in
    let node, entry =
      match inst with
      | Putchar e ->
        let reg = new_reg () in
        let n   = tr_expression e reg id_node in
        IPutchar reg, n
      | Set (_, _) -> assert false
      | If (_c, _e1, _e2) -> assert false
      | While (_c, _e) -> assert false
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
  in

  let params =
    List.map (fun a ->
      let r = new_reg () in
      Hashtbl.add env a (new_reg ());
      r)
    fdef.params
  in

  let entry = List.fold_right (fun inst entry ->
      tr_instruction inst entry
    ) fdef.code (new_reg ())
  in
  {
    name = fdef.name;
    params;
    code;
    entry
  }

