type node  = int
type reg   = int

type ident = string

type address =
  | Addr      of ident
  | AddrStack of int
  | AddrGlobl of ident * int

type operation =
  | OConst of int
  | OAdd  | OMul
  | OLt

type condition =
  | CImm of int
  | CLt

type instruction =
  | INop
  | IPutchar of reg
  | IOp      of operation * reg list * reg * node
  | ILoad    of address * reg * node
  | IStore   of address * reg * node
  | ICall    of ident * reg list * node
  | ICond    of condition * reg list * node * node
  | IReturn  of reg
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

