open Lang.Soya

type error =
  | Type_error               of location * string
  | Undeclared_error         of location option * string
  | Abstract_error of location * string

exception Error of error

(* type error --------------------------------------------------------------- *)

open Lang.Soya

let rec type_to_string = function
  | TInt      -> "int"
  | TChar     -> "char"
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

let not_array e ty =
  raise_type_error e
    (Printf.sprintf
       "This expression has type '%s' but an expression was expected of type\n\
       \t'array'"
       (type_to_string ty))

let not_class e ty =
  raise_type_error e
    (Printf.sprintf
       "This expression has type '%s' but an expression was expected of type\n\
       \t'class'"
       (type_to_string ty))

let number_arguent e nb_exp nb =
  raise_type_error e
    (Printf.sprintf
      "The function call contains the wrong number of elements,\n\
        \t%d instead of %d " nb nb_exp)

(* undeclared *)

let raise_undelcared_error l s =
  raise (Error (Undeclared_error (l,  s)))

let undeclared_function l fname =
  raise_undelcared_error l
    (Printf.sprintf 
      "Function '%s' is does not exist\n" fname)

let undeclared_var l var =
  raise_undelcared_error (Some l)
    (Printf.sprintf
      "Variable '%s' does not exist" var)

let undeclared_class l c =
  raise_undelcared_error (Some l)
    (Printf.sprintf
      "Class '%s' does not exist" c)

(* Abstract ----------------------------------------------------------------- *)

let implement_abstract l =
  raise (Error (Abstract_error (l,
    "You are tring to implement an abstract class")))

let missing_implemen l f =
  let f = List.fold_left
    (fun a (f : 'a function_def) -> Printf.sprintf "%s\n\t- %s" a f.name ) "" f
  in
  raise (Error (Abstract_error (l,
    Printf.sprintf "There are still functions not implemented:%s" f)))

