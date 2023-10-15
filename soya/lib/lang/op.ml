type ident = string

type 'reg address =
  | Addr        of ident
  | AddrReg     of 'reg
  | AddrStack   of int
  | AddrGlobl   of ident

type operation =
  | OConst of int
  | OAddImm of int
  | OMulImm of int
  | OAdd  | OMul
  | OLt

type condition =
  | CEqi of int | CNeqi of int
  | CEq | CNeq
  | CLt | CGe


