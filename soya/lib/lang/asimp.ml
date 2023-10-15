(**
   Annotated abstract syntax for the SIMP language.
   Imp + user-defined structures and arrays, but no explicit pointer manipulation
 *)

(* Types of SIMP values *)
type typ =
  | TInt 
  | TBool
  | TStruct of string (* struct type, identified by its name *)
  | TArray of typ     (* array containing elements of the specified type *)
  | TVoid (* not an actual type in the source language, but having it in
             the AST makes the code more uniform *)

type binop = Imp.binop

type 'a expression = {
  annot: 'a;
  expr: 'a expr;
}
and 'a expr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * 'a expression * 'a expression
  | Call  of string * 'a expression list
  (* No explicit Deref or Alloc *)
  | New   of string            (* create a struct of the given name; fields are not initialized *)
  | NewTab of typ * 'a expression (* create an array of the given type and size *)
  | Read  of 'a mem               (* read in memory *)
and 'a mem =
  | Arr of 'a expression * 'a expression (* array access   e1[e2]   *)
  | Str of 'a expression * string     (* field access    s.x     *)

let mk_expr a e = { annot=a; expr=e }

type 'a instruction =
  | Putchar of 'a expression
  | Set     of string * 'a expression
  | If      of 'a expression * 'a sequence * 'a sequence
  | While   of 'a expression * 'a sequence
  | Return  of 'a expression
  | Expr    of 'a expression
  | Write   of 'a mem * 'a expression (*   m = e;   *)
and 'a sequence = 'a instruction list

(* Function definition is now annotated by types *)
type 'a function_def = {
  name:   string;
  params: (string * typ) list;
  locals: (string * typ) list;
  code:   'a sequence;
  return: typ;
}

(* User-defined structure, with a name and typed fields *)
type struct_def = {
  name:   string;
  fields: (string * typ) list;
}

(* Program as in IMP + types + user-defined structs *)
type 'a program = {
  globals:   (string * typ) list;
  functions: 'a function_def list;
  structs:   struct_def list;
}

