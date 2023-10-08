let imp_to_rtl imp =
  let rtl = Translate.Imp2rtl.tr_program imp in
  if !Option.debug_rtl
  then Debug.PrintRTL.print_rtl rtl !Option.output_file ".rtl0";
  rtl

let call_convention rtl =
  let rtl = Translate.Call_convention.tr_program rtl in
  if !Option.debug_rtl
  then Debug.PrintRTL.print_reg_rtl rtl !Option.output_file ".rtl1";
  rtl

(* let rtl_to_ltl rtl = *)
(*   let ltl = Translate.Rtl2ltl.tr_program rtl in *)
(*   if !Option.debug_ltl *)
(*   then Debug.PrintLTL.print_ltl ltl !Option.output_file ".ltl0"; *)
(*   ltl *)

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
  let _rtl = call_convention rtl in
  (* let _ltl = rtl_to_ltl rtl in *)
  ()

