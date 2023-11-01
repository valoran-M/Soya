open Lang.Soya

type error =
  | Type_error of location * string

exception Error of error

open Lang.Soya


let raise_type_error l s =
  raise (Error (Type_error (l,  s)))

(* type error --------------------------------------------------------------- *)

let rec type_to_string = function
  | TInt      -> "int"
  | TChar     -> "char"
  | TBool     -> "bool"
  | TArray t  -> Printf.sprintf "%s array" (type_to_string t)
  | TClass c  -> String.capitalize_ascii c
  | TParent c -> type_to_string c
  | TVoid     -> "()"

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

let undeclared_function l fname =
  raise_type_error l
    (Printf.sprintf 
      "Function '%s' is does not exist\n" fname)

let undeclared_var l var =
  raise_type_error l
    (Printf.sprintf
      "Variable '%s' does not exist" var)

let undeclared_class l c =
  raise_type_error l
    (Printf.sprintf
      "Class '%s' does not exist" c)

let undeclared_methode l m =
  raise_type_error l
    (Printf.sprintf
      "Method '%s' does not exist" m)

let undeclared_field l f =
  raise_type_error l
    (Printf.sprintf
      "Field '%s' does not exist" f)

(* Abstract ----------------------------------------------------------------- *)

let implement_abstract l =
  raise_type_error l
    "You are tring to implement an abstract class"

let missing_implemen l f =
  let f = List.fold_left
    (fun a (f : 'a function_def) -> Printf.sprintf "%s\n\t- %s" a f.name ) "" f
  in
  raise_type_error l
    (Printf.sprintf "There are still functions not implemented:%s" f)

