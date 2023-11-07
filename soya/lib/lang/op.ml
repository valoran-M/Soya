type ident = string

type s_imm = Label of ident | Cst of int

type static = ident * s_imm list

type mem_size =
  | Word | Byte

type 'reg address =
  | Addr        of ident
  | AddrReg     of 'reg
  | AddrStack   of int
  | AddrGlobl   of ident

type operation =
  | OChar  of int
  | OConst of int
  | OLabel of ident
  | OAddImm of int
  | OSubImm of int
  | OMulImm of int
  | OAdd | OSub
  | OMul
  | OLt

type condition =
  | CEqi of int | CNeqi of int
  | CEq | CNeq
  | CLt | CGe


