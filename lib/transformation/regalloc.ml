open Lang.Rtl
open Lang.Mips

(* Liveness ----------------------------------------------------------------- *)

let register =
  [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9;
   s0; s1; s2; s3; s4; s5; s6; s7]

let caller_saved = [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9]
let callee_saved = [s0; s1; s2; s3; s4; s5; s6; s7]

let get_liveness (rtl_fun : function_def) =
  let def_use = Hashtbl.create 32 in
  let add id def use =
    Hashtbl.add def_use id (def, use)
  in

  let reg = Printf.sprintf "x%d" in

  let fun_args_to_reg =
    let rec aux acc reg args =
      match args, reg with
      | _, [] | [], _ -> acc
      | _ :: args, r :: reg  -> aux (r :: acc) reg args
    in aux [] [a0; a1; a2; a3]
  in

  let rec instruction_get_use id =
    if not (Hashtbl.mem def_use id) then
      match Hashtbl.find rtl_fun.code id with
      | INop n -> instruction_get_use n
      | IPutchar (r, n) ->
        add id [] [reg r];
        instruction_get_use n
      | IMove (rd, r, n) ->
        add id [reg rd] [reg r];
        instruction_get_use n
      | IOp (_, args, rd, n) ->
        add id [reg rd] (List.map reg args);
        instruction_get_use n
      | ILoad (_, rd, n) ->
        add id [reg rd] [];
        instruction_get_use n
      | IStore (_, r, n) ->
        add id [] [reg r];
        instruction_get_use n
      | ICall (_, args, n) ->
        add id caller_saved (fun_args_to_reg args);
        instruction_get_use n
      | ICond (_, _, nt, nf) ->
        instruction_get_use nt;
        instruction_get_use nf
      | IReturn (Some _) ->
        add id [] (v0 :: callee_saved)
      | IReturn None -> ()
      | IGoto n -> instruction_get_use n
  in
  instruction_get_use rtl_fun.entry;
  def_use

