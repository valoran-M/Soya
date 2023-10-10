open Lang
open Lang.Mips

let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let tr_function (fdef : Linear.function_def) =
  let one_arg args =
    match args with
    | x :: _ -> x
    | _ -> assert false
  in

  let two_args args =
    match args with
    | x1 :: x2 :: _ -> x1, x2
    | _ -> assert false
  in

  let tr_op op args r =
    match op with
    | Rtl.OConst n -> li r n
    | Rtl.OAdd -> let r1, r2 = two_args args in add r r1 r2
    | Rtl.OMul -> let r1, r2 = two_args args in mul r r1 r2
    | Rtl.OLt  -> let r1, r2 = two_args args in slt r r1 r2
  in
  
  let tr_cond c args l =
    match c with
    | Rtl.CEqi i  -> let r = one_arg args in beqi r i l
    | Rtl.CNeqi i -> let r = one_arg args in bnei r i l
    | Rtl.CEq   -> let r1, r2 = two_args args in beq  r1 r2 l
    | Rtl.CNeq  -> let r1, r2 = two_args args in bne r1 r2 l
    | Rtl.CLt   -> let r1, r2 = two_args args in blt r1 r2 l
    | Rtl.CGe   -> let r1, r2 = two_args args in bge r1 r2 l
  in

  let tr_load a r =
    match a with
    | Rtl.Addr l      -> la r l @@ lw r 0 r
    | Rtl.AddrGlobl l -> la r l @@ lw r 0 r
    | Rtl.AddrStack i -> lw r i sp
  in
  let tr_store a r =
    match a with
    | Rtl.Addr l      -> la t9 l @@ sw r 0 t9
    | Rtl.AddrGlobl l -> la t9 l @@ sw r 0 t9
    | Rtl.AddrStack i -> sw r i sp
  in

  let tr_instruction (i: Linear.instruction) =
    match i with
    | Linear.LLabel l -> label l
    | Linear.LPutchar r ->
       move a0 r
       @@ li v0 11
       @@ syscall
    | Linear.LMove (rd, r) -> move rd r
    | Linear.LLoad (a, l) -> tr_load a l
    | Linear.LStore (a, l) -> tr_store a l
    | Linear.LPush r -> push r
    | Linear.LOp (op, args, r) -> tr_op op args r
    | Linear.LCond (c, args, l) -> tr_cond c args l
    | Linear.LCall l -> jal l
    | Linear.LGoto l -> b l
    | Linear.LReturn ->
      (if fdef.stack_size <> 0
      then addi sp sp fdef.stack_size
      else nop)
      @@ addi sp fp (-4)
      @@ pop ra
      @@ pop fp
      @@ jr ra
  in
  let code =
    List.fold_left (fun code i -> code @@ tr_instruction i) nop fdef.code
  in
  push fp
  @@ push ra
  @@ addi fp sp 4 @@
  (if fdef.stack_size <> 0
  then subi sp sp fdef.stack_size
  else nop)
  @@ code
  

let gen_prog (prog : Lang.Linear.program) : program =

  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ label "init_end"
    @@ move a0 v0
    @@ jal "main"
    @@ li v0 10
    @@ syscall
  and built_ins =
    comment "built-in atoi"
    @@ label "atoi"
    @@ li   v0 0
    @@ label "atoi_loop"
    @@ lbu  t0 0 a0
    @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error"
    @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10
    @@ add  v0 v0 t0
    @@ addi a0 a0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr   ra
  in


  let function_codes = List.fold_right
    (fun (fdef : Linear.function_def) code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  let text = init @@ function_codes @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  {text; data}

