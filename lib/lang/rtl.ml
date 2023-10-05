type node  = int

type reg =
  | Real   of Mips.register
  | Pseudo of int

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
  | CLt

type instruction =
  | INop     of node
  | IPutchar of reg * node
  | IMove    of reg * reg * node
  | IOp      of operation * reg list * reg * node
  | ILoad    of address * reg * node
  | IStore   of address * reg * node
  | ICall    of ident * reg list * node
  | ICond    of condition * reg list * node * node
  | IReturn  of reg option
  | IGoto    of node

type code = (node, instruction) Hashtbl.t

type function_def = {
    name       : string;
    params     : reg list;
    code       : code;
    entry      : node;
  }

type program = {
    globals   : string list;
    functions : function_def list;
  }

