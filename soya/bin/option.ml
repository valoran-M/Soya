let usage = "usage: soya <file>"

let input_file  = ref ""
let output_file = ref ""

type lang_compile =
  | Imp
let lang_compile = ref Imp

let debug_imp = ref false
let debug_rtl = ref false
let debug_ltl = ref false
let debug_lin = ref false

let debug_list = [debug_imp; debug_rtl; debug_ltl; debug_lin;
                  Debug.PrintRegAlloc.debug]

let spec = [
  ("-Imp ", Arg.Unit (fun () -> lang_compile := Imp), "Imp language compiler");
  
  ("-dimp", Arg.Set debug_imp, "Save generated Imp");
  ("-drtl", Arg.Set debug_rtl, "Save generated RTL");
  ("-dltl", Arg.Set debug_ltl, "Save generated LTL");
  ("-dreg", Arg.Set Debug.PrintRegAlloc.debug, "DEbug Reg alloc");
  ("-dlin", Arg.Set debug_lin, "Save generated Lin");
  ("-dall", Arg.Unit (fun () -> List.iter (fun r -> r := true) debug_list),
            "Save alle debug");

  ("-o", Arg.Set_string output_file , "Name of output file");
]

let set_input_file f =
  input_file := f

let parse_args () =
  Arg.parse (Arg.align spec) set_input_file usage;
  if not (Sys.file_exists !input_file)
  then raise (Arg.Bad "Input file does not exists");
  if !output_file = ""
  then output_file := Filename.remove_extension !input_file ^ ".asm";
  Debug.PrintRegAlloc.file := !output_file
 
