open Format
open Lexing

let print_prog_lines c fl ll fc lc =
  let make_alig i =
    String.make
      (int_of_float (log10 (float_of_int ll))
      - int_of_float (log10 (float_of_int i)))
      ' '
  in
  let line = input_line c in
  let start = String.make fc '.' in
  eprintf
    "%s%d | %s%s@."
    (make_alig fl) fl start
    (String.sub line fc (String.length line - fc));
  for i = fl + 1 to ll - 1 do
    eprintf "%s%d | %s@." (make_alig i) i (input_line c)
  done;
  let line = input_line c in
  let start = String.sub line 0 lc in
  eprintf "%d | %s%s@." ll start (String.make (String.length line - lc) '.')

let print_prog f (fl, ll, fc, lc) color =
  let c = open_in f in
  (* got to line fl *)
  for _ = 1 to fl - 1 do
    ignore (input_line c)
  done;
  if fl = ll then (
    if fc <> lc then (
      eprintf "%d | %s@." fl (input_line c);
      eprintf "%s" (String.make (4 + fc) ' ');
      eprintf "@{<bold>@{<%s>%s@}@}@." color (String.make (lc - fc) '^')
    )
  ) else
    print_prog_lines c fl ll fc lc

let report file (fl, ll, fc, lc) =
  if fl <> ll then
    eprintf
      "@{<bold>File \"%s\", line %d-%d, characters %d-%d:@}\n"
      file fl ll fc lc
  else
    eprintf "@{<bold>File \"%s\", line %d, characters %d-%d:@}\n" file fl fc lc

let pose_lex ps pe =
  (ps.pos_lnum, pe.pos_lnum, ps.pos_cnum - ps.pos_bol, pe.pos_cnum - pe.pos_bol)

let warn_color = "fg_mag"

let err_color = "fg_red"

let print_info file pos color =
  report file pos;
  print_prog file pos color

let print_file file =
  eprintf "@{<bold>File \"%s\"@}\n" file

(* Error print -------------------------------------------------------------- *)

let print_error_loc  file (loc : Lang.Soya.location) s =
  let pose = pose_lex loc.fc loc.lc in
  print_info file pose err_color;
  eprintf "@{<bold>@{<fg_red>Error@}@}: @[%s@]@." s

let print_undelcared file loc s =
  match loc with
  | Some loc -> print_error_loc file loc s
  | None ->
    print_file file;
    eprintf "@{<bold>@{<fg_red>Error@}@}: @[%s@]@." s

let print_error err file =
  Color.add_ansi_marking err_formatter;
  match err with
  | Error.Type_error (e, s)       -> print_error_loc file e s
  | Error.Undeclared_error (l, s) -> print_undelcared file l s
  | Error.Abstract_error (l, s)   -> print_error_loc file l s

