open Lang
open Lang.Asimp

let tr_program (prog : typ program) : Lang.Imp.program =
  let get_struct st =
    List.find (fun s -> s.name = st) prog.structs
  in

  let rec type_size (t : typ) =
    match t with
    | TInt | TBool -> 4
    | TVoid -> 0
    | TArray t -> type_size t
    | TStruct s ->
      let s = get_struct s in
      List.fold_left (fun a t -> a + type_size (snd t)) 0 s.fields
  in

  let get_struct_offset s f =
    let s = get_struct s in
    let f, _ = List.fold_left (fun (found, o) (field, t) ->
      match found with
      | Some _ -> found, o
      | None ->
        if field = f
        then Some o, o
        else (None, o + type_size t))
    (None, 0) s.fields in
    match f with
    | Some o -> o
    | None -> assert false
  in

  let rec tr_expression (e : typ expression) =
    match e.expr with
    | Cst c               -> Imp.Cst c
    | Bool b              -> Imp.Bool b
    | Var v               -> Imp.Var v
    | Binop (op, e1, e2)  -> Imp.Binop (op, tr_expression e1, tr_expression e2)
    | Call (f, a)         -> Imp.Call (f, List.map tr_expression a)
    | New _               -> Imp.Alloc (Imp.Cst (type_size e.annot))
    | Read m              -> Imp.Deref (tr_mem m)
    | NewTab (t, e)       ->
      Imp.Alloc (Imp.Binop (Mul, tr_expression e, Imp.Cst (type_size t)))
  and tr_mem (m : typ mem) =
    match m with
    | Arr (a, o) ->
      let case_size = type_size a.annot in
      Imp.Binop (Add, tr_expression a,
      Imp.Binop (Mul, Cst case_size, tr_expression o))
    | Str (st, f) ->
      match st.annot with
      | TStruct s ->
        Imp.Binop (Add, Cst (get_struct_offset s f), tr_expression st)
      | _ -> assert false
  in

  let rec tr_instruction (i : typ instruction) =
    match i with
    | Putchar c       -> Imp.Putchar (tr_expression c)
    | Set (s, e)      -> Imp.Set (s, tr_expression e)
    | If (c, e1, e2)  -> Imp.If (tr_expression c, tr_seq e1, tr_seq e2)
    | While (c, e)    -> Imp.While (tr_expression c, tr_seq e)
    | Return e        -> Imp.Return (tr_expression e)
    | Expr e          -> Imp.Expr (tr_expression e)
    | Write (m, e)    -> Write (tr_mem m, tr_expression e)
  and tr_seq s =
    List.map tr_instruction s
  in

  let tr_function (fdef : typ function_def) : Lang.Imp.function_def =
    {
      name = fdef.name;
      params = List.map fst fdef.params;
      locals = List.map fst fdef.locals;
      code = tr_seq fdef.code;
    }
  in

  let struct_size = Hashtbl.create 16 in
  List.iter (fun (s : struct_def) ->
    Hashtbl.replace struct_size s.name ()
  ) prog.structs;
  {
    globals = List.map fst prog.globals;
    static  = [];
    functions = List.map tr_function prog.functions;
  }

