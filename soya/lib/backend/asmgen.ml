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
    | OChar n,   []       -> li r n
    | OConst n,  []       -> li r n
    | OLabel l,  []       -> la r l
    | OAddImm n, [r1]     -> if r = r1 && n = 0 then nop else addi r r1 n
    | OSubImm n, [r1]     -> if r = r1 && n = 0 then nop else subi r r1 n
    | OMulImm n, [r1]     -> if r = r1 && n = 1 then nop else muli r r1 n
    | OAdd,      [r1; r2] -> add r r1 r2
    | OSub,      [r1; r2] -> sub r r1 r2
    | OMul,      [r1; r2] -> mul r r1 r2
    | OLt,       [r1; r2] -> slt r r1 r2
    | OOr,       [r1; r2] -> or_ r r1 r2
    | OAnd,      [r1; r2] -> and_ r r1 r2
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

  let size_load size =
    match size with
    | Word -> lw
    | Byte -> lbu
  in
  let tr_load a r s =
    match a with
    | Addr l          -> la r l @@ (size_load s) r 0 r
    | AddrReg ra      -> (size_load s) r 0 ra
    | AddrOReg (i,ra) -> (size_load s) r i ra
    | AddrGlobl l     -> la r l @@ (size_load s) r 0 r
    | AddrStack i     -> (size_load s) r i sp
  in

  let size_store size =
    match size with
    | Word -> sw
    | Byte -> sb
  in
  let tr_store a r s =
    match a with
    | Addr l          -> la t9 l @@ (size_store s) r 0 t9
    | AddrReg ra      -> (size_store s) r 0 ra
    | AddrOReg (i,ra) -> (size_store s) r i ra
    | AddrGlobl l     -> la t9 l @@ (size_store s) r 0 t9
    | AddrStack i     -> (size_store s) r i sp
  in

  let tr_call a =
    match a with
    | Addr i        -> jal i
    | AddrReg r     -> jalr r
    | AddrOReg (i,r)-> addi r r i @@ jalr r
    | AddrStack i   -> addi t8 sp i @@ jalr t8
    | AddrGlobl i   -> jal i
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
    | Linear.LLoad (a, s, l) -> tr_load a l s
    | Linear.LStore (a, s, l) -> tr_store a l s
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
    S "\n"
    @@ comment
      " built-in functions \
        -----------------------------------------------------------\n"
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
  
    @@ S "\n"
    @@ comment " a0 : class descriptor of e"
    @@ comment " a1 : class descriptor of C"
    @@ comment " returun : e instanceof C"
    @@ label "instanceof"
    @@ lw a0 0 a0
    @@ beq a0 a1 "instanceof$true"
    @@ bnez a0 "instanceof"
    @@ li v0 0
    @@ jr ra
    @@ label "instanceof$true"
    @@ li v0 1
    @@ jr ra

    @@ S "\n\n"
  in


  let function_codes = List.fold_right
    (fun (fdef : Linear.function_def) code ->
      S "\n" @@ label fdef.name @@ tr_function fdef @@ code)
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


