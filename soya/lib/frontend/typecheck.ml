open Classe
open Lang.Soya

module Env = Map.Make(String)

let type_check (prog : location program) =
  let envc = Hashtbl.create 16 in

(* utils functions  --------------------------------------------------------- *)

  let get_class n =
    match Hashtbl.find_opt envc n with
    | Some c -> c
    | None   -> failwith "Class doesn't exist"
  in

  let rec get_class_name t =
    match t with
    | TClass s -> s
    | TParent c -> get_class_name c
    | _ -> failwith "Is not a class"
  in

  let get_function f loc =
    match List.find_opt
      (fun (e : 'a function_def) -> e.name = f) prog.functions with
    | Some f -> f
    | None   -> Error_soy.Error.undeclared_function (Some loc) f
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


(* type checker ------------------------------------------------------------- *)

  let check_type l t exp =
    match t, exp with
    | TInt, TChar | TChar, TInt -> ()
    | _, TVoid -> ()
    | _ ->
      if t = exp
      then ()
      else Error_soy.Error.type_error l t exp
  in

(* AST typer ---------------------------------------------------------------- *)

  let rec type_expr exp (expr : location expression) env : typ expression=
    match expr.expr with
    | Char c          -> check_type expr.annot TChar exp; mk_expr exp (Char c)
    | Cst c           -> type_const exp expr.annot c
    | Bool b          -> mk_expr exp (Bool b)
    | Var v           -> type_var v expr.annot env
    | Binop ((Add | Mul as op), e1, e2) ->
      let e1t = type_expr TInt e1 env in
      let e2t = type_expr TInt e2 env in
      check_type e1.annot e1t.annot TInt; check_type e2.annot e2t.annot TInt;
      mk_expr TInt (Binop (op, e1t, e2t))
    | Binop (Lt as op, e1, e2) ->
      let e1t = type_expr TInt e1 env in
      let e2t = type_expr TInt e2 env in
      check_type e1.annot e1t.annot TInt; check_type e2.annot e2t.annot TInt;
      mk_expr TBool (Binop (op, e1t, e2t))
    | Call (f, args) -> type_function f args expr.annot env
    | MCall (c, f, args) -> type_method c f args env
    | New (c, args) -> type_constructor c args env
    | NewTab (t, s) ->
      let st = type_expr TInt s env in
      mk_expr t (NewTab (t, st))
    | Read m  -> type_read m env
    | This    -> mk_expr (type_var "this" expr.annot env).annot This
    | Super   -> mk_expr (type_var "super" expr.annot env).annot Super

  and type_var v loc env =
    mk_expr (get_var_type v loc env) (Var v)

  and type_const exp loc c =
    match exp with
    | TChar -> mk_expr exp (Char c)
    | _     -> check_type loc TInt exp; mk_expr exp (Cst c)

  and type_args args (f : location function_def) env =
    try
      List.fold_left2 (fun n arg (_, t) ->
          let e = type_expr t arg env in
          e :: n
        ) [] args f.params
    with _ -> failwith "arguments list has not the same size"

  and type_function f args loc env =
    let f = get_function f loc in
    let args = type_args args f env in
    mk_expr f.return (Call (f.name, args))

  and type_method c f args env =
    let c = type_expr TVoid c env in
    let s = get_class_name c.annot in
    let m = get_method envc s f in
    let args = type_args args m env in
    mk_expr m.return (MCall (c, f, args))

  and type_constructor cn args env =
    let c = get_class cn in
    if c.abstract then failwith "Class is abstract";
    let m = get_method envc cn "constructor" in
    let args = type_args args m env in
    mk_expr (TClass cn) (New (cn, args))

  and type_read m env =
    match m with
    | Arr (a, i) ->
      let at = type_expr TVoid a env in
      let t = get_array_type at.annot a.annot in
      let it = type_expr TInt i env in
      mk_expr (TArray t) (Read (Arr (at, it)))
    | Atr (c, f) ->
      let c = type_expr TVoid c env in
      let s = get_class_name c.annot in
      let _, t = get_field envc s f in
      mk_expr t (Read (Atr (c, f)))
  in

  let rec type_instruction i (ret: typ) env : typ instruction =
    match i with
    | Putchar e ->
      let et = type_expr TInt e env in
      Putchar et
    | Set (s, sloc, e) ->
      let t  = get_var_type s sloc env in
      let et = type_expr t e env in
      Set (s, sloc, et)
    | If (c, e1, e2) ->
      let ct = type_expr TBool c env in
      If (ct, type_sequence e1 ret env, type_sequence e2 ret env)
    | While (c, e) ->
      let ct = type_expr TBool c env in
      While (ct, type_sequence e ret env)
    | Return e ->
      let et = type_expr ret e env in
      Return et
    | Expr e -> Expr (type_expr TVoid e env)
    | Write (m, e) ->
      match m with
      | Arr (a, i) ->
        let it = type_expr TInt i env in
        check_type i.annot it.annot TInt;
        let at = type_expr TVoid a env in
        let t = get_array_type at.annot a.annot in
        let et = type_expr t e env in
        check_type e.annot et.annot t;
        Write (Arr(at, it), et)
      | Atr (c, f) ->
        let c = type_expr TVoid c env in
        let s = get_class_name c.annot in
        let _, t = get_field envc s f in
        let et = type_expr t e env in
        check_type e.annot et.annot t;
        (Write (Atr (c, f), et))
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
      else failwith "some methods are not implemented"
  in

  let type_class (c : location class_def) =
    let env = Env.add "this" (TClass c.name) env in
    let env, c =
      match c.parent with
      | None   -> env, c
      | Some cn ->
        let pc = get_class cn in
        let c = if pc.abstract then check_abstract pc c else c in
        Env.add "super" (TParent (TClass cn)) env, c
    in
    Hashtbl.add envc c.name c;
    let methods = List.map (type_function env) c.methods in
    let fields = merge_fields envc c.name [] in
    { c with fields; methods; abs_methods = [] }
  in

(* Prog --------------------------------------------------------------------- *)

  let classes   = List.map type_class prog.classes in
  let functions = List.map (type_function env) prog.functions in
  { prog with functions; classes }

