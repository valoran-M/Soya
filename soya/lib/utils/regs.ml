open Lang.Rtl
open Lang.Mips

let caller_saved = List.map (fun r -> Real r)
  [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9; a0; a1; a2; a3; v0; v1]
let callee_saved = List.map (fun r -> Real r)
  [ra; s0; s1; s2; s3; s4; s5; s6; s7]

let register =
  [t0; t1; t2; t3; t4; t5; t6; t7;
   s0; s1; s2; s3; s4; s5; s6; s7; v0; v1]

