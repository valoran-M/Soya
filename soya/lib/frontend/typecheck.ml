open Classe
open Lang.Soya

module Env = Map.Make(String)

let type_check (prog : location program) =
  let (envc : (string, location class_def) Hashtbl.t) = Hashtbl.create 16 in

(* utils functions  --------------------------------------------------------- *)

  let get_classe n loc =
    match Hashtbl.find_opt envc n with
    | Some c -> c
    | None   -> Error_soy.Error.undeclared_class loc n
  in

  let get_classe_name t loc =
    match t with
    | TClass s -> s
    | _ -> Error_soy.Error.not_class loc t
  in

  let get_function f loc =
    match List.find_opt
      (fun (e : 'a function_def) -> e.name = f) prog.functions with
    | Some f -> f
    | None   -> Error_soy.Error.undeclared_function loc f
  in

  let get_array_type a loc =
    match a with
    | TArray t -> t
    | _ -> Error_soy.Error.not_array loc a
  in

  let get_var_type v loc env =
    match Env.find_opt v env with
    | Some v -> v
    | None   -> Error_soy.Error.undeclared_var loc v
  in

  let unsafe_get_classe t =
    match t with
    | TClass s -> Hashtbl.find envc s
    | _ -> assert false
  in


(* type checker ------------------------------------------------------------- *)
  let instanceof c cexp loc =
    let cexp = get_classe cexp loc in
    let rec aux c =
      let c = get_classe c loc in
      if c.name = cexp.name then true
      else
        match c.parent with
        | None -> false
        | Some (p, _) -> aux p
    in
    aux c
  in

  let check_type l t exp =
    match t, exp with
    | TChar, TInt | TInt, TChar -> ()
    | TClass c, TClass cexp -> 
      if instanceof c cexp l
      then ()
      else Error_soy.Error.type_error l t exp
    | _, TVoid -> ()
    | _ ->
      if t = exp
      then ()
      else Error_soy.Error.type_error l t exp
  in

  let rec down_cast_condition (c : typ expression) loc (envt, envf) =
    match c.expr with
    | Binop (And, c1, c2)    ->
      down_cast_condition c2 loc (down_cast_condition c1 loc (envt, envf))
    | Instanceof (c, (i, _)) ->
      (match c.expr with
      | Var v ->
        let c = unsafe_get_classe c.annot in
        if not (instanceof c.name i loc) then
          (Env.add v (TClass i) envt), envf
        else envt, envf
      | _ -> envt, envf)
    | _ -> envt, envf
 in

(* AST typer ---------------------------------------------------------------- *)

  let rec type_expr expr env : typ expression =
    match expr.expr with
    | Char c          -> mk_expr TChar (Char c)
    | Cst c           -> mk_expr TInt  (Cst c)
    | Bool b          -> mk_expr TBool (Bool b)
    | Var v           -> mk_expr (get_var_type v expr.annot env) (Var v)
    | Binop ((Add | Mul | Sub as op), e1, e2) ->
      let e1t = type_expr e1 env in
      let e2t = type_expr e2 env in
      check_type e1.annot e1t.annot TInt; check_type e2.annot e2t.annot TInt;
      mk_expr TInt (Binop (op, e1t, e2t))
    | Binop (Lt as op, e1, e2) ->
      let e1t = type_expr e1 env in
      let e2t = type_expr e2 env in
      check_type e1.annot e1t.annot TInt; check_type e2.annot e2t.annot TInt;
      mk_expr TBool (Binop (op, e1t, e2t))
    | Binop ((And | Or as op), e1, e2) ->
      let e1t = type_expr e1 env in
      let e2t = type_expr e2 env in
      check_type e1.annot e1t.annot TBool; check_type e2.annot e2t.annot TBool;
      mk_expr TBool (Binop (op, e1t, e2t))
    | Instanceof (e, (c, a)) ->
      let cn = (get_classe c a).name in
      let et = type_expr e env in
      (* check if e is a class *)
      ignore (get_classe_name et.annot e.annot);
      mk_expr TBool (Instanceof (et, (c, TClass cn)))
    | Call (f, args)     -> type_function f args expr.annot env
    | MCall (c, f, args) -> type_method c f args expr.annot env
    | New (c, args)      -> type_constructor c args expr.annot env
    | NewTab (t, s) ->
      let st = type_expr s env in
      check_type s.annot st.annot TInt;
      mk_expr (TArray t) (NewTab (t, st))
    | Read m  -> type_read m env
    | This    -> mk_expr (get_var_type "this"  expr.annot env) (Var "this")
    | Super   -> mk_expr (get_var_type "super" expr.annot env) (Var "this")


  and type_args args (f : location function_def) loc env =
    try
      List.fold_left2 (fun n arg (_, t) ->
          let e = type_expr arg env in
          check_type arg.annot e.annot t;
          e :: n)
        [] args f.params
    with Invalid_argument _ ->
      Error_soy.Error.number_arguent loc
        (List.length f.params) (List.length args)

  and type_function f args loc env =
    let f = get_function f loc in
    let args = type_args args f loc env in
    mk_expr f.return (Call (f.name, args))

  and type_method c f args loc env =
    match c.expr with
    | Var v when not (Env.mem v env) -> (* static method *)
      let c = get_classe v c.annot in
      let m = get_static_method envc v f loc in
      let args  = type_args args m loc env in
      mk_expr m.return (Call (f ^ "$" ^ c.name, args))
    | _ ->
      let ct    = type_expr c env in
      let s     = get_classe_name ct.annot c.annot in
      let m     = get_method envc s f loc in
      let args  = type_args args m loc env in
      mk_expr m.return (MCall (ct, f, args))

  and type_constructor cn args loc env =
    let c = get_classe cn loc in
    if c.abstract then Error_soy.Error.implement_abstract loc;
    let m = get_method envc cn "constructor" loc in
    let args = type_args args m loc env in
    mk_expr (TClass cn) (New (cn, args))

  and type_read m env =
    match m with
    | Arr (a, i) ->
      let at = type_expr a env in
      let t  = get_array_type at.annot a.annot in
      let it = type_expr i env in
      check_type i.annot it.annot TInt;
      mk_expr (TArray t) (Read (Arr (at, it)))
    | Atr (c, f, floc) ->
      let ct  = type_expr c env in
      let s   = get_classe_name ct.annot c.annot in
      let t   = get_field envc s f floc in
      mk_expr (snd t) (Read (Atr (ct, f, floc)))
  in

  let rec type_instruction i (ret: typ) env : typ instruction =
    match i with
    | Putchar e ->
      let et = type_expr e env in
      check_type e.annot et.annot TChar;
      Putchar et
    | Set (s, sloc, e) ->
      let t  = get_var_type s sloc env in
      let et = type_expr e env in
      check_type e.annot et.annot t;
      Set (s, sloc, et)
    | If (c, e1, e2) ->
      let ct = type_expr c env in
      check_type c.annot TBool ct.annot;
      let envt, envf = down_cast_condition ct c.annot (env, env) in
      If (ct, type_sequence e1 ret envt, type_sequence e2 ret envf)
    | While (c, e) ->
      let ct = type_expr c env in
      check_type c.annot TBool ct.annot;
      While (ct, type_sequence e ret env)
    | Return e ->
      let et = type_expr e env in
      check_type e.annot et.annot ret;
      Return et
    | Expr e -> Expr (type_expr e env)
    | Write (m, e) ->
      let et  = type_expr e env in
      match m with
      | Arr (a, i) ->
        let it = type_expr i env in
        check_type i.annot it.annot TInt;
        let at  = type_expr a env in
        (* check if it's an array *)
        let t   = get_array_type at.annot a.annot in
        check_type e.annot et.annot t;
        Write (Arr(at, it), et)
      | Atr (c, f, floc) ->
        let ct= type_expr c env in
        let s = get_classe_name ct.annot c.annot in
        let t = snd (get_field envc s f floc)   in
        check_type e.annot et.annot t;
        (Write (Atr (ct, f, floc), et))
  and type_sequence s ret env =
    List.map (fun s -> type_instruction s ret env) s
  in


(* env ---------------------------------------------------------------------- *)

  let add_vars v env =
    List.fold_left (fun env (g, t) -> Env.add g t env) env v
  in

  let env = add_vars prog.globals Env.empty in

  let type_function env (f : location function_def) : typ function_def =
    let env = add_vars f.locals (add_vars f.params env) in
    let code = type_sequence f.code f.return env in
    { f with code }
  in

(* type class --------------------------------------------------------------- *)

  let check_abstract c_parent c =
    let abs_methods = c_parent.abs_methods in
    let abs_methods =
      List.fold_left (fun abs (m : 'a function_def) ->
        List.filter (fun (a : 'a function_def) -> a.name <> m.name) abs )
      abs_methods c.methods
    in
    if c.abstract
    then { c with abs_methods }
    else
      if abs_methods = [] then c
      else Error_soy.Error.missing_implemen c.loc abs_methods
  in

  let type_class (c : location class_def) =
    let static = List.map (type_function env) c.static in
    let env = Env.add "this" (TClass c.name) env in
    let env, c =
      match c.parent with
      | None -> env, c
      | Some (cn, loc) ->
        let pc = get_classe cn loc in
        let c = if pc.abstract then check_abstract pc c else c in
        Env.add "super" (TClass pc.name) env, c
    in
    Hashtbl.add envc c.name c;
    let methods = List.map (type_function env) c.methods in
    let fields = merge_fields envc c.name [] in
    { c with fields; methods; static; abs_methods = [] }
  in

(* Prog --------------------------------------------------------------------- *)

  let classes   = List.map type_class prog.classes in
  let functions = List.map (type_function env) prog.functions in
  { prog with functions; classes }

