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
    | IOp (op, args, r, n)-> (LOp (op, args, r)) :: lin n code
    | IPutchar (r, n)     -> LPutchar r :: lin n code
    | IMove (r1, r2, n)   -> LMove (r1, r2) :: lin n code
    | ILoad (a, r, n)     -> LLoad (a, r) :: lin n code
    | IStore (a, r, n)    -> LStore (a, r) :: lin n code
    | IPush (r, n)        -> LPush r :: lin n code
    | ICall (i, _, n)     -> LCall i :: lin n code
    | IReturn _           -> (LReturn :: code)
    | IGoto n             -> lin n code
    | ICond (c, lr, nt, nf) ->
      let lt = Hashtbl.find_opt id_to_label nt in
      let lf = Hashtbl.find_opt id_to_label nf in
      match lt, lf with
      | Some lt, Some _  -> Hashtbl.replace label_is_used lt ();
                            LCond (c, lr, lt) :: lin nf code
      | Some lt, None    -> LCond (c, lr, lt) :: lin nf code
      | None,    Some lf -> Hashtbl.replace label_is_used lf ();
                            LCond (negate_condition c, lr, lf) :: lin nt code
      | None,    None    ->
        let codet = lin nt code in
        let codef = lin nf code in
        let code = codef @ codet in
        let lt = Hashtbl.find id_to_label nt in
        Hashtbl.replace label_is_used lt ();
        LCond (c, lr, Hashtbl.find id_to_label nt) :: code
  in

  let rec remove_usless_label code =
    match code with
    | [] -> []
    | LLabel l :: c ->
      if Hashtbl.mem label_is_used l
      then LLabel l :: remove_usless_label c
      else remove_usless_label c
    | i :: c -> i :: remove_usless_label c
  in
  {
    stack_size = fdef.stack_size;
    name = fdef.name;
    code = remove_usless_label (lin fdef.entry []);
  }

let linearize prog : Linear.program =
  {
    globals   = prog.globals;
    functions = List.map (fun f -> tr_function f) prog.functions;
  }
