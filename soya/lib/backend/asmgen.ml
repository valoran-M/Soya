open Lang
open Lang.Mips
open Lang.Op

let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let tr_function (fdef : Linear.function_def) =
  let tr_op op args r =
    match op, args with
    | OConst n,  []       -> li r n
    | OAddImm n, [r1]     -> addi r r1 n
    | OMulImm n, [r1]     -> addi r r1 n
    | OAdd,      [r1; r2] -> add r r1 r2
    | OMul,      [r1; r2] -> mul r r1 r2
    | OLt,       [r1; r2] -> slt r r1 r2
    | _ -> assert false
  in
  
  let tr_cond c args l =
    match c, args with
    | CEqi i,  [r]      -> beqi r i l
    | CNeqi i, [r]      -> bnei r i l
    | CEq,     [r1; r2] -> beq  r1 r2 l
    | CNeq,    [r1; r2] -> bne r1 r2 l
    | CLt,     [r1; r2] -> blt r1 r2 l
    | CGe,     [r1; r2] -> bge r1 r2 l
    | _ -> assert false
  in

  let tr_load a r =
    match a with
    | Addr l      -> la r l @@ lw r 0 r
    | AddrReg ra  -> lw r 0 ra
    | AddrGlobl l -> la r l @@ lw r 0 r
    | AddrStack i -> lw r i sp
  in
  let tr_store a r =
    match a with
    | Addr l      -> la t9 l @@ sw r 0 t9
    | AddrReg ra  -> sw r 0 ra
    | AddrGlobl l -> la t9 l @@ sw r 0 t9
    | AddrStack i -> sw r i sp
  in

  let tr_call a =
    match a with
    | Addr i      -> jal i
    | AddrReg r   -> jalr r
    | AddrStack i -> addi t8 sp i @@ jalr t8
    | AddrGlobl i -> jal i
  in

  let tr_instruction (i: Linear.instruction) =
    match i with
    | Linear.LLabel l -> label l
    | Linear.LPutchar _ ->
      li v0 11
      @@ syscall
    | Linear.LAlloc _ ->
      li v0 9
      @@ syscall
    | Linear.LMove (rd, r) -> move rd r
    | Linear.LLoad (a, l) -> tr_load a l
    | Linear.LStore (a, l) -> tr_store a l
    | Linear.LPush r -> push r
    | Linear.LOp (op, args, r) -> tr_op op args r
    | Linear.LCond (c, args, l) -> tr_cond c args l
    | Linear.LCall a -> tr_call a
    | Linear.LGoto l -> b l
    | Linear.LReturn ->
      (if fdef.stack_size <> 0
      then addi sp sp fdef.stack_size
      else nop)
      @@ jr ra
  in
  let code =
    List.fold_left (fun code i -> code @@ tr_instruction i) nop fdef.code
  in
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
    (fun id code -> label id @@ dword [Mips.Int 0] @@ code)
    prog.globals nop
  in

  let data = List.fold_left
    (fun d (id, s) -> d @@ label id @@ dword (
      List.map (fun (s : Op.s_imm) ->
        match s with
        | Label id -> Mips.Label id
        | Cst i -> Mips.Int i
      ) s)
    ) data prog.static
  in
  {text; data}


