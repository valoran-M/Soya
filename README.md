# Soya

A compiler from Soya to Mips

The documentation of Soya language is [here](./doc/soya.md)

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

1. [Soya](./soya/lib/lang/soya.ml) Soya language
1. [Imp](./soya/lib/lang/imp.ml) A simple imperative language.
1. [RTL](./soya/lib/lang/rtl.ml) A graph a representation of Imp with pseudos and
   physicals registers with 3-address operations.
1. [LTL](./soya/lib/lang/ltl.ml) Like RTL but with only physicals registers.
1. [Linear](./soya/lib/lang/linear.ml) A linear representation of LTL

### Pass

1. [type check](./soya/lib/frontend/typecheck.ml) Creates typed Soya AST.
1. [Soya2Imp](./soya/lib/frontend/soya2imp.ml) Translate Soya to an simple
   iterative language (Imp).
1. [Imp2RTL](./soya/lib/backend/imp2rtl.ml) Construction of the CFG, 3-address
   code generation.
1. [Const propagation](./soya/lib/backend/constprop.ml) Propagate constant with
   a static analysis.
1. [Call Convention](./soya/lib/backend/call_convention.ml) Use real register
   to respect Call Convention.
1. [Dead node elimination](./soya/lib/backend/dead_node.ml.ml) Remove useless
   node when register destination is never used.
1. [RTL2LTL](./soya/lib/backend/rtl2ltl.ml) Register allocation
1. [linearize](./soya/lib/backend/linearize.ml) Linearization of the CFG
1. [Asm Gen](./soya/lib/backend/asmgen.ml) Mips assembly code generation

## Sources

1. Jean-Christophe Filliâtre's compilation course [poly](https://www.lri.fr/~filliatr/pub/lpc.pdf)
1. Constant propagation with conditionals branches [article](https://dl.acm.org/doi/pdf/10.1145/103135.103136)

## Thanks

1. [Gurvan](https://gitlab.com/Gurvan.dev) for its help in allocating registers
   part

