open Lang.Soya

type error =
  | Type_error of location * string

exception Error of error

(* type error --------------------------------------------------------------- *)

open Lang.Soya

let rec type_to_string = function
  | TInt      -> "int"
  | TBool     -> "bool"
  | TArray t  -> Printf.sprintf "%s array" (type_to_string t)
  | TClass c  -> String.capitalize_ascii c
  | TParent c -> type_to_string c
  | TVoid     -> "()"

let raise_type_error l s =
  raise (Error (Type_error (l,  s)))

let type_error e ty ty_exp =
  raise_type_error e
    (Printf.sprintf
       "This expression has type '%s' but an expression was expected of type\n\
       \t'%s'"
       (type_to_string ty)
       (type_to_string ty_exp))


