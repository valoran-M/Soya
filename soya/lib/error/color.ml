open Format

type style =
  | FG_Red
  | FG_Mag
  | FG_Default
  | Normal
  | Bold
  | Bold_off

let mark_open_stag = function
  | String_tag s -> "<" ^ s ^ ">"
  | _ -> ""

let mark_close_stag = function
  | String_tag s -> "</" ^ s ^ ">"
  | _ -> ""

let print_open_stag = ignore

let print_close_stag = ignore

let close_tag = function
  | Bold -> Bold_off
  | FG_Red -> FG_Default
  | FG_Mag -> FG_Default
  | _ -> Normal

let style_of_tag = function
  | String_tag s -> (
    match s with
    | "n" -> Normal
    | "bold" -> Bold
    | "/bold" -> Bold
    | "fg_red" -> FG_Red
    | "fg_mag" -> FG_Mag
    | "fg_default" -> FG_Default
    | _ -> raise Not_found)
  | _ -> raise Not_found

let to_ansi_value = function
  | Normal -> "0"
  | Bold -> "1"
  | Bold_off -> "22"
  | FG_Red -> "31"
  | FG_Mag -> "35"
  | FG_Default -> "39"

let ansi_tag = Printf.sprintf "\x1B[%sm"

let start_mark_ansi_stag t = ansi_tag @@ to_ansi_value @@ style_of_tag t

let stop_mark_ansi_stag t =
  ansi_tag @@ to_ansi_value @@ close_tag @@ style_of_tag t

let add_ansi_marking formatter =
  pp_set_mark_tags formatter true;
  let old_fs = pp_get_formatter_stag_functions formatter () in
  pp_set_formatter_stag_functions formatter
  { old_fs with
    mark_open_stag  = start_mark_ansi_stag;
    mark_close_stag = stop_mark_ansi_stag; }

