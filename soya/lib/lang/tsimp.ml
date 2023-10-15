(**
   Abstract syntax for the SIMP language.
   Imp + user-defined structures and arrays, but no explicit pointer manipulation
 *)

type expression = {
  typ: Simp.typ;
  expr: expr_descr;
}
and expr_descr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of Simp.binop * expression * expression
  | Call  of string * expression list
  (* No explicit Deref or Alloc *)
  | New   of string            (* create a struct of the given name; fields are not initialized *)
  | NewTab of Simp.typ * expression (* create an array of the given type and size *)
  | Read  of mem               (* read in memory *)
and mem =
  | Arr of expression * expression (* array access   e1[e2]   *)
  | Str of expression * string     (* field access    s.x     *)

let mk_expr t e = { typ=t; expr=e }

type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  | Write   of mem * expression (*   m = e;   *)
and sequence = instruction list

(* Function definition is now annotated by types *)
type function_def = {
  name:   string;
  params: (string * Simp.typ) list;
  locals: (string * Simp.typ) list;
  code:   sequence;
  return: Simp.typ;
}

(* Program as in IMP + types + user-defined structs *)
type program = {
    globals:   (string * Simp.typ) list;
    functions: function_def list;
    structs:   Simp.struct_def list;
}

