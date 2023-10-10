val input_file  : string ref
val output_file : string ref
 
type lang_compile =
  | Imp
val lang_compile : lang_compile ref

val debug_imp : bool ref
val debug_rtl : bool ref
val debug_ltl : bool ref
val debug_lin : bool ref

val parse_args : unit -> unit

