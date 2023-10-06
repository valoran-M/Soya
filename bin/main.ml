let imp_to_rtl imp =
  let rtl = Translate.Imp2rtl.tr_program imp in
  if !Option.debug_rtl
  then Debug.PrintRTL.print_rtl rtl !Option.output_file ".rtl";
  rtl

let () =
  Option.parse_args ();
  let c = open_in !Option.input_file in
  let lb = Lexing.from_channel c in
  let _rtl = 
    let open Lex in
    match !Option.lang_compile with
    | Imp ->
      let prog = Impparser.program Implexer.token lb in
      imp_to_rtl prog
  in
  ()

