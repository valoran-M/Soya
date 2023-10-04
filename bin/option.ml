let usage = "usage: soya <file>"

let input_file  = ref ""
let output_file = ref None

type lang_compile =
  | Imp
let lang_compile = ref Imp

let debug_imp = ref false
let debug_rtl = ref false

let spec = [
  ("-Imp ", Arg.Unit (fun () -> lang_compile := Imp), "Imp language compiler");
  
  ("-dimp", Arg.Set debug_imp, "Save generated Imp");
  ("-drtl", Arg.Set debug_imp, "Save generated RTL");

  ("-o", Arg.String (fun s -> output_file := Some s), "Name of output file");
]

let set_input_file f =
  if not (Sys.file_exists f)
  then raise (Arg.Bad "Input file does not exists")
  else input_file := f

let parse_args () =
  Arg.parse (Arg.align spec)  set_input_file usage

