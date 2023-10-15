open Format

let imp_to_rtl imp =
  let rtl = Backend.Imp2rtl.tr_program imp in
  if !Option.debug_rtl
  then Debug.PrintRTL.print_rtl rtl !Option.output_file ".rtl0";
  rtl

let call_convention rtl =
  let rtl = Backend.Call_convention.tr_program rtl in
  if !Option.debug_rtl
  then Debug.PrintRTL.print_reg_rtl rtl !Option.output_file ".rtl1";
  rtl

let rtl_to_ltl rtl =
  let ltl = Backend.Rtl2ltl.tr_program rtl in
  if !Option.debug_ltl
  then Debug.PrintLTL.print_ltl ltl !Option.output_file ".ltl0";
  ltl

let lin_ltl ltl =
  let line = Backend.Linearize.linearize ltl in
  if !Option.debug_lin
  then Debug.PrintLinear.print_lin line !Option.output_file ".lin";
  line

let () =
  Option.parse_args ();
  let c = open_in !Option.input_file in
  let lb = Lexing.from_channel c in
  let rtl = 
    let open Lex in
    match !Option.lang_compile with
    | Imp ->
      let prog = Impparser.program Implexer.token lb in
      imp_to_rtl prog
  in
  let rtl = call_convention rtl in
  let ltl = rtl_to_ltl rtl in
  let lin = lin_ltl ltl in
  let asm = Backend.Asmgen.gen_prog lin in

  let file = Filename.remove_extension !Option.output_file ^ ".asm" in
  let out = open_out file in
  let outf = formatter_of_out_channel out in
  Lang.Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0


