open Lang.Rtl
open Lang.Mips

let fun_args_to_reg reg =
  let rec aux acc regs args =
    match regs, args with
    | _,         []        -> acc
    | [],        a :: args -> aux ((reg a :: acc)) [] args
    | r :: reg,  _ :: args -> aux (r :: acc) reg args
  in
  aux [] [a0; a1; a2; a3]

let nb_args_to_reg nb =
  match nb with
  | 0 -> Real a0 | 1 -> Real a1
  | 2 -> Real a2 | 3 -> Real a3
  | _ -> assert false

let tr_function (fdef : pseudo function_def) : pseudo_reg function_def =
  (* RTL code *)
  let code : (node, pseudo_reg instruction) Hashtbl.t = Hashtbl.create 16 in
  let env = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let nb_reg  = ref fdef.nb_reg in
  let new_reg, new_node =
    let node = ref (-1) in
    (fun () -> incr nb_reg;  Pseu !nb_reg ), (fun () -> incr node; !node)
  in

  let id_env = Hashtbl.create 16 in
  let push_node node =
    let id = new_node () in
    Hashtbl.replace code id node;
    id
  in

  List.iter (fun r -> Hashtbl.replace env r (new_reg ())) Regs.callee_saved;

  (* Translate ---------------------------------------------------------------*)

  let reg r =
    match r with
    | Pseudo r -> Pseu r
  in

  let push_callee_save dest =
    List.fold_left (fun d r ->
      push_node (IMove (Hashtbl.find env r, r, d)))
      dest Regs.callee_saved
  in

  let pop_callee_save dest =
    List.fold_right (fun r d -> push_node (IMove (r, Hashtbl.find env r, d)))
      Regs.callee_saved dest
  in

  let set_args dest =
    fst (List.fold_left (fun (dest, i) r ->
      if i < 4
      then push_node (IMove (reg r, nb_args_to_reg i, dest)), i+1
      else (dest, i+1))
      (dest, 0) fdef.params
    )
  in

  let tr_call id args dest =
    let nb_args = List.length args in
    let dest = push_node (ICall (id, [], nb_args, dest)) in
    fst (List.fold_left (fun (dest, i) r ->
      if i < 4 then (push_node (IMove(nb_args_to_reg i, reg r, dest)),i+1)
               else (push_node (IPush(reg r, dest)),i+1)
      ) (dest, 0) args)
  in

  let rec tr_instruction i =
    match Hashtbl.find_opt id_env i with
    | Some i' -> i'
    | None ->
      let inst = Hashtbl.find fdef.code i in
      let nid = new_node () in
      Hashtbl.add id_env i nid;
      let bid = match inst with
      | INop n           -> push_node (INop (tr_instruction n))
      | IGoto n          -> push_node (IGoto (tr_instruction n))
      | IPutchar(r,n)    -> push_node (IPutchar (reg r, tr_instruction n))
      | IMove(rd,r,n)    -> push_node (IMove (reg rd, reg r, tr_instruction n))
      | IOp(op,rl,r,n)   -> push_node (IOp(op,List.map reg rl,reg r,tr_instruction n))
      | ILoad(a,r,n)     -> push_node (ILoad (a, reg r, tr_instruction n))
      | IStore(a,r,n)    -> push_node (IStore(a, reg r, tr_instruction n))
      | IPush(a,n)       -> push_node (IPush(reg a, tr_instruction n))
      | IPop(a,n)        -> push_node (IPop(reg a, tr_instruction n))
      | ICall(id,lr,_,n) -> tr_call id lr (tr_instruction n)
      | ICond(c,lr,nt,nf) ->
        push_node
          (ICond(c, List.map reg lr, tr_instruction nt, tr_instruction nf))
      | IReturn None     ->
        let dest = push_node (IReturn None) in
        let id = pop_callee_save dest in
        Hashtbl.replace id_env i id; id
      | IReturn (Some r) ->
        let dest = push_node (IReturn (Some (reg r))) in
        let id = pop_callee_save dest in
        Hashtbl.replace id_env i id; id
      in
      let node = Hashtbl.find code bid in
      Hashtbl.remove code bid;
      Hashtbl.replace code nid node;
      nid
  in

  let entry = push_callee_save (set_args (tr_instruction fdef.entry)) in
  {
    nb_reg = !nb_reg;
    name = fdef.name;
    params = [];
    code;
    entry;
  }

let tr_program (prog : pseudo Lang.Rtl.program) : pseudo_reg Lang.Rtl.program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

