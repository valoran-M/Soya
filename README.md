# Soya

A compiler from Soya to Mips

## Args

- `-o [file]` Name of output file

### language

- `-Imp` : Compilation of Imp file

### debug

- `-dimp` : Save generated Imp
- `-drtl` : Save generated RTL
- `-dltl` : Save generated LTL
- `-dlin` : Save generated linearize
- `-dall` : activate all debug

## Compiler pass

### Languages

1. [Imp](./soya/lib/lang/imp.ml) A simple imperative language.
1. [RTL](./soya/lib/lang/rtl.ml) A graph a representation of Imp with pseudos and
   physicals registers with 3-address operations.
1. [LTL](./soya/lib/lang/ltl.ml) Like RTL but with only physicals registers.
1. [Linear](./soya/lib/lang/linear.ml) A linear representation of LTL

### Pass

1. [Imp2RTL](./soya/lib/backend/imp2rtl.ml) Construction of the CFG, 3-address
   code generation.
1. [Call Convention](./soya/lib/backend/call_convention.ml) Use real register
   to respect Call Convention
1. [RTL2LTL](./soya/lib/backend/rtl2ltl.ml) Register allocation
1. [linearize](./soya/lib/backend/linearize.ml) Linearization of the CFG
1. [Asm Gen](./soya/lib/backend/asmgen.ml) Mips assembly code generation

