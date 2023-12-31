open Op

type node  = int

type reg = Mips.register

type instruction =
  | IPutchar of reg * node
  | IAlloc   of reg * node
  | IMove    of reg * reg * node
  | IOp      of operation * reg list * reg * node
  | ILoad    of reg address * reg * mem_size * node
  | IStore   of reg address * reg * mem_size * node
  | ICall    of reg Op.address * int * node
  | ICond    of condition * reg list * node * node
  | IReturn  of reg option
  | IGoto    of node

type code = (node, instruction) Hashtbl.t

type function_def = {
    stack_size : int;
    name       : string;
    code       : code;
    entry      : node;
  }

type program = {
    globals   : string list;
    static    : Op.static list;
    functions : function_def list;
  }

