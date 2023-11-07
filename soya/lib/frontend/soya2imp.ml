open Lang
open Lang.Soya

let tr_program (prog : typ program) : Lang.Imp.program =
  let get_class st =
    List.find (fun s -> s.name = st) prog.classes
  in

  let env = ref [] in
  let new_var =
    let i = ref 0 in
    fun c -> incr i;
      let n = c ^ string_of_int !i in
      env := n :: !env; n
  in

  let rec type_to_class t =
    match t with
    | TClass c -> get_class c
    | TParent c -> type_to_class c
    | _ -> assert false
  in

  let rec type_size (t : typ) =
    match t with
    | TChar -> 1
    | TInt | TBool -> 4
    | TVoid     -> 0
    | TArray t  -> type_size t
    | TClass _  -> 4
    | TParent _ -> 4
  in

  let field_size (c : typ class_def) =
    List.fold_left (fun a (_, t) -> a + type_size t ) 4 c.fields
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

  let envm = Hashtbl.create 16 in
  let get_method_offset c m =
    let c = type_to_class c in
    let methods = Classe.merge_methods envm prog c in
    let f, _ = List.fold_left (fun (found, o) (_, (field : typ function_def)) ->
      match found with
      | Some _ -> found, o
      | None ->
        if field.name = m
        then Some o, o
        else (None, o + 4))
    (None, 4) methods in
    match f with
    | Some o -> o
    | None -> assert false
  in

  let rec tr_expression e : Imp.expression * (string * Soya.typ) list =
    match e.expr with
    | Char c              -> Char c, []
    | Cst c               -> Cst c,  []
    | Bool b              -> Bool b, []
    | Var v               -> Var v,  []
    | Binop (op, e1, e2)  ->
      let e1, d1 = tr_expression e1 in
      let e2, d2 = tr_expression e2 in
      Binop (op, e1, e2), d1 @ d2
    | Call (f, a)         -> let a, d = tr_args a in Call (f, a), d
    | This                -> Var "this", []
    | Super               -> Var "this", []
    | MCall (o, s, args)  -> tr_mcall o s args
    | Read m              -> let m, s, d = tr_mem m in Deref (m, s), d
    | Instanceof(e,(c,_)) ->
      let e, d = tr_expression e in
      Call ("instanceof",
        [e; Addr (c ^ "$descriptor")]), d
    | New (c, args)       ->
      let a, d = tr_args args in
      let v = new_var c in
      Call("constructor$"^c, Var v :: a), (v, TClass c) :: d
    | NewTab (t, e)       ->
      let e, d= tr_expression e in
      Alloc(Binop(Mul,e,Cst(type_size t))), d

  and tr_args a =
    List.fold_left (fun (a, d) e ->
      let e, de = tr_expression e in
      (e :: a, de @ d)
    ) ([],[]) a

  and tr_mcall o s args =
    match o.expr with
    | Super | This ->
      let c = type_to_class o.annot in
      let a, d = tr_args args in
      Call (s ^ "$" ^ c.name, Var "this" :: a), d
    | _ ->
      let c, dc = tr_expression o in
      let a, da = tr_args args in
      let f = Imp.Binop (Add, deref_method_call o.annot c,
                              Cst (get_method_offset o.annot s)) in
      Imp.DCall (Deref (f, Word), c :: a), dc @ da
  
  and deref_method_call t c =
    match t with
    | TClass _  -> Imp.Deref (c, Word)
    | TParent p -> Imp.Deref (deref_method_call p c, Word)
    | _ -> assert false

  and tr_mem (m : typ mem) =
    match m with
    | Arr (a, o) ->
      let case_size = type_size a.annot in
      let a, da = tr_expression a in
      let o, dob = tr_expression o in
      let size = if case_size = 1 then Op.Byte else Op.Word in
      Binop (Add, a,
      Binop (Mul, Cst case_size, o)), size, da @ dob
    | Atr (st, f, _) ->
      match st.annot with
      | TClass s ->
        let st, dst = tr_expression st in
        Binop (Add, Cst (get_class_offset s f), st), Word, dst
      | _        -> assert false
  in

  let instr_to_seq d i =
    List.fold_left (fun a (v, d) ->
      let c = type_to_class d in
      let size = field_size (get_class c.name) in
      Imp.Set(v, Alloc (Cst size))
      :: Write(Var v, Word, Addr(c.name ^ "$" ^ "descriptor")) :: a
    ) [i] d
  in

  let rec tr_instruction (i : typ instruction) : Imp.sequence =
    match i with
    | Putchar c ->
      let c, d = tr_expression c in
      instr_to_seq d (Putchar c)
    | Set (s, _, e) ->
      let e, d = tr_expression e in
      instr_to_seq d (Set (s, e))
    | If (c, e1, e2) ->
      let c, d = tr_expression c in
      instr_to_seq d (If (c, tr_seq e1, tr_seq e2))
    | While (c, e)   ->
      let c, d = tr_expression c in
      instr_to_seq d (While (c, tr_seq e))
    | Return e       ->
      let e, d = tr_expression e in
      instr_to_seq d (Return e)
    | Expr e         ->
      let e, d = tr_expression e in
      instr_to_seq d (Expr e)
    | Write (m, e)   ->
      let m, s, dm = tr_mem m in
      let e, de = tr_expression e in
      instr_to_seq (dm @ de) (Write (m, s, e))
  and tr_seq s =
    List.fold_left (fun s i -> s @ tr_instruction i) [] s
  in

  let tr_class (c : typ class_def) f : Imp.function_def list =
    let tr_method (f : typ function_def) : Imp.function_def =
      let code = tr_seq f.code in
      let code =
        if f.name = "constructor"
        then code @ [Return (Var "this")]
        else code
      in
      let nv = !env in
      env := [];
      { name = f.name ^ "$" ^ c.name;
        params = "this" :: List.map fst f.params;
        locals =  nv @ List.map fst f.locals;
        code; }
      in
    List.fold_left (fun f m -> tr_method m :: f) f c.methods
  in

  let tr_function (fdef : typ function_def) : Lang.Imp.function_def =
    let code = tr_seq fdef.code in
    let nv = !env in
    env := [];
    {
      name = fdef.name;
      params = List.map fst fdef.params;
      locals =  nv @ List.map fst fdef.locals;
      code;
    }
  in

  let static : Op.static list =
    List.map (fun (c : typ class_def) ->
      
      (c.name ^ "$descriptor",
        (match c.parent with
        | None   -> Op.Cst 0
        | Some (c, _) -> Op.Label (c ^ "$descriptor")) ::
          (List.fold_right
            (fun (n, (m : typ function_def)) s ->
              Op.Label (m.name ^ "$" ^ n) :: s)
            (Classe.merge_methods envm prog c) []))
    ) prog.classes
  in

  {
    globals = List.map fst prog.globals;
    static  = static;
    functions = List.fold_left (fun f c -> tr_class c f)
      (List.map tr_function prog.functions) prog.classes;
  }

