type node  = int

type reg = Mips.register

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
  | IPush    of reg * node
  | IPop     of reg * node
  | ICall    of ident * reg list * int * node
  | ICond    of condition * reg list * node * node
  | IReturn  of reg option
  | IGoto    of node

type code = (node, instruction) Hashtbl.t

type function_def = {
    stack_size : int;
    nb_reg     : int;
    name       : string;
    code       : code;
    entry      : node;
  }

type program = {
    globals   : string list;
    functions : function_def list;
  }

