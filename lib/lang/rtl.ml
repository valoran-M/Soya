type node  = int

type pseudo =
  | Pseudo of int

type pseudo_reg =
  | Pseu of int
  | Real of Mips.register

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
  | CImm of int
  | CEqi of int | CNeqi of int
  | CEq | CNeq
  | CLt | CGe

type 'reg instruction =
  | INop     of node
  | IPutchar of 'reg * node
  | IMove    of 'reg * 'reg * node
  | IOp      of operation * 'reg list * 'reg * node
  | ILoad    of address * 'reg * node
  | IStore   of address * 'reg * node
  | IPush    of 'reg * node
  | IPop     of 'reg * node
  | ICall    of ident * 'reg list * int * node
  | ICond    of condition * 'reg list * node * node
  | IReturn  of 'reg option
  | IGoto    of node

type 'reg code = (node, 'reg instruction) Hashtbl.t

type 'reg function_def = {
    nb_reg     : int;
    name       : string;
    params     : 'reg list;
    code       : 'reg code;
    entry      : node;
  }

type 'reg program = {
    globals   : string list;
    functions : ('reg function_def) list;
  }

