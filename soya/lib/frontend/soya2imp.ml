open Lang
open Lang.Soya

let tr_program (prog : typ program) : Lang.Imp.program =
  let get_class st =
    List.find (fun s -> s.name = st) prog.classes
  in

  let type_to_class t =
    match t with
    | TClass c -> get_class c
    | _ -> assert false
  in

  let rec type_size (t : typ) =
    match t with
    | TInt | TBool -> 4
    | TVoid -> 0
    | TArray t -> type_size t
    | TClass _ -> 4
  in

  let get_class_offset s f =
    let s = get_class s in
    let f, _ = List.fold_left (fun (found, o) (field, t) ->
      match found with
      | Some _ -> found, o
      | None ->
        if field = f
        then Some o, o
        else (None, o + type_size t))
    (None, 4) s.fields in
    match f with
    | Some o -> o
    | None -> assert false
  in

  let rec tr_expression (e : typ expression) : Imp.expression =
    match e.expr with
    | Cst c               -> Cst c
    | Bool b              -> Bool b
    | Var v               -> Var v
    | Binop (op, e1, e2)  -> Binop (op, tr_expression e1, tr_expression e2)
    | Call (f, a)         -> Call (f, List.map tr_expression a)
    | Read m              -> Deref (tr_mem m)
    | This                -> Var "this"
    | New (c, args)       -> Call("constructor$"^c,List.map tr_expression args)
    | NewTab (t, e)       -> Alloc(Binop(Mul,tr_expression e,Cst(type_size t)))
    | MCall (o, s, args)  ->
      let args = tr_expression o :: (List.map tr_expression args) in
      let c_name = (type_to_class o.annot).name in
      Imp.Call (s ^ "$" ^ c_name, args)
  and tr_mem (m : typ mem) : Imp.expression =
    match m with
    | Arr (a, o) ->
      let case_size = type_size a.annot in
      Binop (Add, tr_expression a,
      Binop (Mul, Cst case_size, tr_expression o))
    | Atr (st, f) ->
      match st.annot with
      | TClass s ->
        Binop (Add, Cst (get_class_offset s f), tr_expression st)
      | _ -> assert false
  in

  let rec tr_instruction (i : typ instruction) : Imp.instruction =
    match i with
    | Putchar c       -> Putchar (tr_expression c)
    | Set (s, e)      -> Set (s, tr_expression e)
    | If (c, e1, e2)  -> If (tr_expression c, tr_seq e1, tr_seq e2)
    | While (c, e)    -> While (tr_expression c, tr_seq e)
    | Return e        -> Return (tr_expression e)
    | Expr e          -> Expr (tr_expression e)
    | Write (m, e)    -> Write (tr_mem m, tr_expression e)
  and tr_seq s =
    List.map tr_instruction s
  in

  let tr_class (c : typ class_def) f : Imp.function_def list =
    let tr_method (f : typ function_def) : Imp.function_def =
      if f.name = "constructor" then 
        let size = type_size (TClass c.name) in
        let code : Imp.sequence =
          Imp.Set("this", Alloc (Cst size))
          :: Write(Var "this", Var(c.name ^ "$" ^ "descriptor"))
          :: tr_seq f.code @ [Return (Var "this")]
        in
        { name = f.name ^ "$" ^ c.name;
          params = List.map fst f.params;
          locals = "this" :: List.map fst f.locals;
          code; }
      else
        { name = f.name ^ "$" ^ c.name;
          params = "this" :: List.map fst f.params;
          locals = List.map fst f.locals;
          code = tr_seq f.code; }
    in
    List.fold_left (fun f m -> tr_method m :: f) f c.methods
  in

  let tr_function (fdef : typ function_def) : Lang.Imp.function_def =
    {
      name = fdef.name;
      params = List.map fst fdef.params;
      locals = List.map fst fdef.locals;
      code = tr_seq fdef.code;
    }
  in

  let static : Op.static list =
    List.map (fun (c : typ class_def) ->
      (c.name ^ "$descriptor", (Op.Cst 0) ::
           (List.fold_right
              (fun (m : typ function_def) s ->
                Op.Label (m.name ^ "$" ^ c.name) :: s)
           c.methods []))
    ) prog.classes
  in

  {
    globals = List.map fst prog.globals;
    static  = static;
    functions = List.fold_left (fun f c -> tr_class c f)
      (List.map tr_function prog.functions) prog.classes;
  }
