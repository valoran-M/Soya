open Lang.Rtl

module Env = Map.Make(
  struct
    type t = pseudo
    let compare t1 t2 =
      match (t1, t2) with
      | Pseudo r1, Pseudo r2 -> Int.compare r1 r2
  end
)

let tr_function (fdef : pseudo function_def) : pseudo function_def =
  (* RTL code *)
  let code : (node, pseudo instruction) Hashtbl.t = Hashtbl.create 16 in
  let _env = Hashtbl.create 16 in

  (* Pseudo register and node interface *)
  let nb_reg  = ref fdef.nb_reg in
  let _new_reg, new_node =
    let node = ref (-1) in
    (fun () -> incr nb_reg;  Pseu !nb_reg ), (fun () -> incr node; !node)
  in

  let id_env = Hashtbl.create 16 in
  let push_node node =
    let id = new_node () in
    Hashtbl.replace code id node;
    id
  in

  (* Translate ---------------------------------------------------------------*)

  let rec tr_instruction i env =
    match Hashtbl.find_opt id_env i with
    | Some i' -> i'
    | None ->
      let nid = new_node () in
      Hashtbl.add id_env i nid;
      let bid = match Hashtbl.find fdef.code i with
      | INop n           -> push_node (INop (tr_instruction n env))
      | IGoto n          -> push_node (IGoto (tr_instruction n env))
      | IPutchar(r,n)    -> push_node (IPutchar(r, tr_instruction n env))
      | IMove(rd,r,n)    -> push_node (IMove (rd,r, tr_instruction n env))
      | IOp(op,rl,r,n)   -> push_node (IOp(op,rl,r,tr_instruction n env))
      | ILoad(a,r,n)     -> push_node (ILoad (a,r, tr_instruction n env))
      | IStore(a,r,n)    -> push_node (IStore(a,r, tr_instruction n env))
      | ICall(id,lr,i,r,n)-> push_node (ICall(id,lr,i,r, tr_instruction n env))
      | ICond(c,lr,nt,nf) ->
        push_node (ICond(c,lr, tr_instruction nt env, tr_instruction nf env))
      | IReturn None     -> push_node (IReturn None)
      | IReturn (Some r) -> push_node (IReturn (Some r))
      | _ -> assert false
      in
      let node = Hashtbl.find code bid in
      Hashtbl.remove code bid;
      Hashtbl.replace code nid node;
      nid
  in

  let entry = tr_instruction fdef.entry Env.empty in
  {
    nb_reg = !nb_reg;
    name = fdef.name;
    params = [];
    code;
    entry;
    max_pushed_args = 0;
  }

let tr_program (prog : pseudo Lang.Rtl.program) : pseudo Lang.Rtl.program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }

