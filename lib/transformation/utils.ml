open Lang.Mips

let fun_args_to_reg reg =
  let rec aux acc regs args =
    match regs, args with
    | _,         []        -> acc
    | [],        a :: args -> aux ((reg a :: acc)) [] args
    | r :: reg,  _ :: args -> aux (r :: acc) reg args
  in
  aux [] [a0; a1; a2; a3]

let nb_args_to_reg nb =
  match nb with
  | 0 -> a0 | 1 -> a1
  | 2 -> a2 | 3 -> a3
  | _ -> assert false
