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
  | 0 -> a0 | 1 -> a1
  | 2 -> a2 | 3 -> a3
  | _ -> assert false

let tr_function (fdef : pseudo function_def) : pseudo_reg function_def =
  (* RTL code *)
  let code = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let nb_reg  = ref fdef.nb_reg in
  let _new_reg, new_node =
    let node = ref (-1) in
    (fun () -> incr nb_reg;  Pseudo !nb_reg ), (fun () -> incr node; !node)
  in

  let push_node node =
    let id = new_node () in
    Hashtbl.replace code id node;
    id
  in

  (* Translate ---------------------------------------------------------------*)

  let reg r =
    match r with
    | Pseudo r -> Pseu r
  in

  let rec tr_instruction i =
    if Hashtbl.mem fdef.code i then
      let inst = Hashtbl.find fdef.code i in
      Hashtbl.remove fdef.code i;
      match inst with
      | INop n         -> push_node (INop (tr_instruction n))
      | IPutchar(r,n)  -> push_node (IPutchar (reg r, tr_instruction n))
      | IMove(rd,r,n)  -> push_node (IMove (reg rd, reg r, tr_instruction n))
      | IOp(op,rl,r,n) -> push_node (IOp(op,List.map reg rl,reg r,tr_instruction n))
      | ILoad(a,r,n)   -> push_node (ILoad (a, reg r, tr_instruction n))
      | IStore(a,r,n)  -> push_node (IStore(a, reg r, tr_instruction n))
      | IPush(a,n)     -> push_node (IPush(reg a, tr_instruction n))
      | IPop(a,n)      -> push_node (IPop(reg a, tr_instruction n))
      | IGoto n        -> push_node (IGoto (tr_instruction n))
      | ICond(c,lr,nt,nf) ->
        push_node (
          ICond(c, List.map reg lr, tr_instruction nt, tr_instruction nf))
      | IReturn None     -> push_node (IReturn None)
      | IReturn (Some r) -> push_node (IReturn (Some (reg r)))
      | ICall (id,lr,n)  -> push_node (ICall (id, List.map reg lr, tr_instruction n))
    else (-1)
  in
  let entry = tr_instruction (fdef.entry) in
  {
    nb_reg = !nb_reg;
    name = fdef.name;
    params = List.map reg fdef.params;
    code;
    entry;
  }


let tr_program (prog : pseudo Lang.Rtl.program) : pseudo_reg Lang.Rtl.program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

