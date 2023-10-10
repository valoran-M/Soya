type ident = string

type address =
  | Addr      of ident
  | AddrStack of int
  | AddrGlobl of ident

type operation =
  | OConst of int
  | OAdd  | OMul
  | OLt

type condition =
  | CEqi of int | CNeqi of int
  | CEq | CNeq
  | CLt | CGe


