open Lang.Rtl
open Regalloc

let tr_program (prog : pseudo_reg program) =
  let tr_function (f : pseudo_reg function_def) =
    let _, in_out, _ = get_liveness f in
    let code  = Hashtbl.create 16 in

    let ppf = Debug.PrintRegAlloc.gen_ppf f.name ".cliveness" in
    Debug.PrintRegAlloc.print_liveness ppf in_out Reg_set.fold;
    Debug.PrintRegAlloc.close_ppf ppf;



    (* Pseudo register and node interface *)
    let new_node =
      let node = ref (-1) in
      fun () -> incr node; !node
    in

    let id_env = Hashtbl.create 16 in
    let push_node node =
      let id = new_node () in
      Hashtbl.replace code id node;
      id
    in

    let rec tr_instruction i =
      match Hashtbl.find_opt id_env i with
      | Some i' -> i'
      | None    ->
        let nid = new_node () in
        Hashtbl.add id_env i nid;
        let bid = match Hashtbl.find f.code i with
          | INop n            -> push_node (INop (tr_instruction n))
          | IGoto n           -> push_node (IGoto (tr_instruction n))
          | IPutchar(r,n)     -> push_node (IPutchar (r,tr_instruction n))
          | ILoad(a,r,s,n)    -> push_node (ILoad (a,r,s,tr_instruction n))
          | IAlloc(r,d,n)     -> push_node (IAlloc (r,d,tr_instruction n))
          | IStore(a,r,s,n)   -> push_node (IStore(a,r,s,tr_instruction n))
          | ICall(id,lr,a,r,n)-> push_node (ICall (id,lr,a,r,tr_instruction n))
          | IReturn r         -> push_node (IReturn r)
          | ISetParam(r,i,p,n)-> push_node (ISetParam (r,i,p, tr_instruction n))
          | IGetParam(r,i,p,n)-> push_node (IGetParam (r,i,p, tr_instruction n))
          (* const_prop *)
          | IMove(rd,r,n)     -> tr_move rd r i (tr_instruction n)
          | IOp(op,rl,r,n)    -> tr_op op rl r i (tr_instruction n)
          | ICond(c,lr,nt,nf) ->
            push_node (ICond (c,lr,tr_instruction nt,tr_instruction nf))
        in
        let node = Hashtbl.find code bid in
        Hashtbl.remove code bid;
        Hashtbl.replace code nid node;
        nid
    and tr_move rd r i dest =
      let _, out = Hashtbl.find in_out i in
      if Reg_set.mem rd out
      then push_node (IMove(rd, r, dest))
      else dest
    and tr_op op rl rd i dest =
      let _, out = Hashtbl.find in_out i in
      if Reg_set.mem rd out
      then push_node (IOp (op, rl, rd, dest))
      else dest
    in

    let entry = tr_instruction f.entry in
    { f with code; entry; }
  in
    

  { prog with functions = List.map tr_function prog.functions }
