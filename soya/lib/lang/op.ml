type ident = string

type s_imm = Label of ident | Cst of int

type static = ident * s_imm list

type mem_size =
  | Word | Byte

type 'reg address =
  | Addr        of ident
  | AddrReg     of 'reg
  | AddrOReg    of int * 'reg
  | AddrStack   of int
  | AddrGlobl   of ident

type operation =
  | OChar  of int
  | OConst of int
  | OLabel of ident
  | OAddImm of int
  | OSubImm of int
  | OMulImm of int
  | ONeg
  | ONot
  | OAdd | OSub
  | OMul
  | OLt  | OLe
  | OGt  | OGe
  | OEq  | ONeq
  | OAnd | OOr

type condition =
  | CConst of int
  | CEqi   of int | CNeqi of int
  | CEq | CNeq
  | CLt | CLe
  | CGe | CGt

