open Lang.Rtl

type const =
  | Ninit | Const of int | NConst

module Env = Map.Make(
  struct
    type t = pseudo
    let compare t1 t2 =
      match (t1, t2) with
      | Pseudo r1, Pseudo r2 -> Int.compare r1 r2
  end
)

let init_const (f : pseudo function_def) =
  let inst_regs (i : pseudo instruction) a =
    match i with
    | IOp (_, args, r, _) -> r :: args @ a
    | ILoad (_, r, _, _)
    | IGetParam (r, _, _, _) -> r :: a
    | IMove (rd, r, _) -> rd :: r :: a
    | ICall (_, _, _, Some r, _) -> r :: a
    | _ -> a
  in
  let regs = Hashtbl.fold (fun _ i l -> inst_regs i l) f.code [] in
  let env = List.fold_left (fun e r -> Env.add r NConst e) Env.empty f.params in
  let env = List.fold_left (fun e r -> Env.add r Ninit  e) env regs in

  let in_out = Hashtbl.create (Hashtbl.length f.code) in
  Hashtbl.iter (fun i _ -> Hashtbl.add in_out i (env, env)) f.code;

  let one_reg i r rd f c_in =
    match Env.find r c_in with
    | Ninit   -> Some (rd, Ninit)
    | Const c -> Some (rd, (Const (f i c)))
    | NConst  -> Some (rd, NConst)
  in
  
  let two_reg r1 r2 rd f c_in =
    match Env.find r1 c_in, Env.find r2 c_in with
    | Ninit, _  | _, Ninit  -> Some (rd, Ninit)
    | NConst, _ | _, NConst -> Some (rd, (NConst))
    | Const c2, Const c1    -> Some (rd, (Const (f c1 c2)))
  in

  let compute_op (op : Lang.Op.operation) rd args c_in =
    let open Lang.Op in
    match op, args with
    | OChar  i,  []   -> Some (rd, (Const i))
    | OConst i,  []   -> Some (rd, (Const i))
    | OLabel _,  []   -> Some (rd, NConst)
    | OAddImm i, [r]  -> one_reg i r rd ( + ) c_in
    | OSubImm i, [r]  -> one_reg i r rd ( - ) c_in
    | OMulImm i, [r]  -> one_reg i r rd ( * ) c_in
    | OAdd, [r1; r2]  -> two_reg r1 r2 rd ( - ) c_in
    | OSub, [r1; r2]  -> two_reg r1 r2 rd ( + ) c_in
    | OMul, [r1; r2]  -> two_reg r1 r2 rd ( * ) c_in
    | OLt,  [r1; r2]  ->
      two_reg r1 r2 rd (fun i1 i2 -> if i1 <= i2 then 1 else 0) c_in
    | OAnd, [r1; r2]  -> two_reg r1 r2 rd ( * ) c_in
    | OOr,  [r1; r2]  -> two_reg r1 r2 rd (fun i1 i2 -> (i1 + i2) mod 2) c_in
    | _ -> assert false
  in

  let evaluate_node (i: pseudo instruction) c_in =
    match i with
    | IOp (op, args, r, _) -> compute_op op r args c_in
    | IMove (rd, r, _)     -> Some (rd, (Env.find r c_in))
    | IGetParam (r, _, _, _) | ILoad (_, r, _, _)
    | ICall (_, _, _, Some r, _) -> Some (r, NConst)
    | _ -> None
  in

  let get_succ (i : pseudo instruction) =
    match i with
    | INop d                 | IGoto d
    | IPutchar (_, d)        | IAlloc (_, _, d)
    | IMove (_, _, d)        | IOp (_, _, _, d)
    | ILoad (_, _, _, d)     | IStore (_, _, _, d)
    | ISetParam (_, _, _, d) | IGetParam (_, _, _, d)
    | ICall (_, _, _, _, d) -> [d]
    | ICond (_, _, d1, d2)  -> [d1; d2]
    | IReturn _             -> []
  in

  (* static file analysis *)
  let file = Queue.create () in
  Hashtbl.iter (fun id _ -> Queue.add id file) f.code;

  let merge_env c d rd =
    let modify = ref false in
    let c_out  = Env.fold (fun p c c_out ->
      if List.mem p rd then c_out
      else
        match Env.find p c_out, c with
        | NConst, _ -> c_out
        | Ninit,  _ ->
          if c = Ninit
          then c_out
          else (modify := true; Env.add p c c_out)
        | Const c1, Const c2 ->
          if c1 = c2
          then c_out
          else (modify := true; Env.add p NConst c_out)
        | Const _, Ninit  -> c_out
        | Const _, NConst -> modify := true; Env.add p NConst c_out
    ) c d in
    !modify, c_out
  in

  let rec const_assgn () =
    match Queue.take_opt file with
    | None    -> ()
    | Some id ->
      let c_in, c_out = Hashtbl.find in_out id in
      let i = Hashtbl.find f.code id in
      let _, c_out =
        match evaluate_node i c_in with
        | None         -> merge_env c_in c_out []
        | Some (rd, v) -> merge_env c_in (Env.add rd v c_out) [rd]
      in
      Hashtbl.replace in_out id (c_in, c_out);
      let succ = get_succ i in
      List.iter (fun sid ->
        let c_in, c_out' = Hashtbl.find in_out sid in
        let m, c_in = merge_env c_out c_in [] in
        if m then (
          Queue.add sid file;
          Hashtbl.replace in_out sid (c_in, c_out'))
      ) succ;
      const_assgn ()
  in
  const_assgn ();
  in_out

let tr_program (prog : pseudo program) =
  let tr_function (f: pseudo function_def) =
    let in_out = init_const f in

    let code  = Hashtbl.create 16 in
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
      | None ->
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
          | ISetParam(r,i,p,n)-> push_node (ISetParam (r,i,p,n))
          | IGetParam(r,i,p,n)-> push_node (IGetParam (r,i,p,n))
          (* const_prop *)
          | IMove(rd,r,n)     -> tr_move i rd r (tr_instruction n)
          | IOp(op,rl,r,n)    -> tr_op i op rl r (tr_instruction n)
          | ICond(c,lr,nt,nf) ->
            push_node (ICond (c,lr,tr_instruction nt,tr_instruction nf))
        in
        let node = Hashtbl.find code bid in
        Hashtbl.remove code bid;
        Hashtbl.replace code nid node;
        nid

    and tr_move id rd r dest =
      let c_in, _ = Hashtbl.find in_out id in
      match Env.find r c_in with
      | Ninit | NConst -> push_node (IMove (rd, r, dest))
      | Const c -> push_node (IOp (Lang.Op.OConst c, [], rd, dest))
    and tr_op id op args rd dest =
      let open Lang.Op in
      let c_in, _ = Hashtbl.find in_out id in

      let one_reg r i f =
        match Env.find r c_in with
        | Ninit | NConst -> push_node (IOp (op, args, rd, dest))
        | Const c -> push_node (IOp (OConst (f c i), [], rd, dest))
      in

      let two_reg r1 r2 f g =
        match Env.find r1 c_in, Env.find r2 c_in with
        | Const c1, Const c2 -> push_node (IOp (OConst (f c1 c2), [], rd, dest))
        | Const c, _         -> push_node (g c r2)
        | _, Const c         -> push_node (g c r1)
        | _, _               -> push_node (IOp (op, args, rd, dest))
      in

      match op, args with
      | OChar _,  []
      | OConst _, []
      | OLabel _, []   -> push_node (IOp (op, args, rd, dest))
      | OAddImm i, [r] -> one_reg r i ( + )
      | OSubImm i, [r] -> one_reg r i ( - )
      | OMulImm i, [r] -> one_reg r i ( * )
      | OAdd, [r1; r2]  ->
        two_reg r1 r2 ( + ) (fun c r -> IOp (OAddImm c, [r], rd, dest))
      | OSub, [r1; r2]  ->
        two_reg r1 r2 ( - ) (fun c r -> IOp (OSubImm c, [r], rd, dest))
      | OMul, [r1; r2]  ->
        two_reg r1 r2 ( * ) (fun c r -> IOp (OMulImm c, [r], rd, dest))
      | OLt,  [r1; r2]  ->
        two_reg r1 r2 (fun i1 i2 -> if i1 <= i2 then 1 else 0)
          (fun _ _ -> IOp (OLt, args, rd, dest))
      | OAnd, [r1; r2]  -> two_reg r1 r2 ( * )(fun _ _ ->(IOp(op,args,rd,dest)))
      | OOr,  [r1; r2]  ->
        two_reg r1 r2 (fun i1 i2 -> (i1 + i2) mod 2) 
          (fun _ _ ->(IOp(op,args,rd,dest)))
      | _ -> assert false
    in

    let entry = tr_instruction f.entry in
    { f with code; entry; }
  in

  { prog with functions = List.map tr_function prog.functions }

