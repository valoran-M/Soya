open Lang
open Linear
open Ltl

let tr_function (fdef : Ltl.function_def) =
  let negate_condition (c: Rtl.condition) =
    match c with
    | Rtl.CImm i -> if i = 0 then Rtl.CImm 1 else Rtl.CImm 0
    | Rtl.CEqi n -> Rtl.CNeqi n
    | Rtl.CNeqi n -> Rtl.CEqi n
    | Rtl.CEq -> Rtl.CNeq
    | Rtl.CNeq -> Rtl.CEq
    | Rtl.CLt -> Rtl.CGe
    | Rtl.CGe -> Rtl.CLt
  in
  let id_to_label   = Hashtbl.create 32 in
  let label_is_used = Hashtbl.create 8  in

  let create_label =
    let count = ref (-1) in
    (fun id ->
      incr count;
      let label = Printf.sprintf "%s_%d" fdef.name !count in
      Hashtbl.replace id_to_label id label; label)
  in

  let rec lin l code =
    match Hashtbl.find_opt id_to_label l with
    | None    -> let label = create_label l in
                 LLabel label :: instr l code
    | Some l  -> Hashtbl.replace label_is_used l ();
                 LGoto l :: code
  and instr l code =
    match (Hashtbl.find fdef.code l) with
    | IOp (op, args, r, n)-> lin n (LOp (op, args, r) :: code)
    | IPutchar (r, n)     -> lin n (LPutchar r :: code)
    | IMove (r1, r2, n)   -> lin n (LMove (r1, r2) :: code)
    | ILoad (a, r, n)     -> lin n (LLoad (a, r) :: code)
    | IStore (a, r, n)    -> lin n (LStore (a, r) :: code)
    | IPush (r, n)        -> lin n (LPush r :: code)
    | ICall (i, _, n)     -> lin n (LCall i :: code)
    | IReturn _           -> (LReturn :: code)
    | IGoto n             ->
      (match Hashtbl.find_opt id_to_label n with
      | Some l -> LGoto l :: code
      | None -> lin n code)
    | ICond (c, lr, nt, nf) ->
      let lt = Hashtbl.find_opt id_to_label nt in
      let lf = Hashtbl.find_opt id_to_label nf in
      match lt, lf with
      | Some lt, Some _  -> Hashtbl.replace label_is_used lt ();
                            LCond (c, lr, lt) :: lin nf code
      | Some lt, None    -> LCond (c, lr, lt) :: lin nf code
      | None,    Some lf -> Hashtbl.replace label_is_used lf ();
                            LCond (negate_condition c, lr, lf) :: lin nt code
      | None,    None    -> let code = lin nt code @ lin nf code in
                            LCond (c, lr, Hashtbl.find id_to_label nt) :: code
  in

  {
    stack_size = fdef.stack_size;
    name = fdef.name;
    code = lin fdef.entry [];
  }


