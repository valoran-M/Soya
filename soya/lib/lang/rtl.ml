open Op

type node  = int

type pseudo =
  | Pseudo of int

type pseudo_reg =
  | Pseu of int
  | Real of Mips.register

type 'reg instruction =
  | INop      of node
  | IPutchar  of 'reg * node
  | IAlloc    of 'reg * 'reg option * node
  | IMove     of 'reg * 'reg * node
  | IOp       of operation * 'reg list * 'reg * node
  | ILoad     of 'reg address * 'reg * mem_size * node
  | IStore    of 'reg address * 'reg * mem_size * node
  | ISetParam of 'reg * int * int * node
  | IGetParam of 'reg * int * int * node
  | ICall     of 'reg Op.address * 'reg list * int * 'reg option * node
  | ICond     of condition * 'reg list * node * node
  | IReturn   of 'reg option
  | IGoto     of node

type 'reg code = (node, 'reg instruction) Hashtbl.t

type 'reg function_def = {
    nb_reg     : int;
    name       : string;
    params     : 'reg list;
    code       : 'reg code;
    entry      : node;
    max_pushed_args : int;
  }

type 'reg program = {
    globals   : string list;
    static    : Op.static list;
    functions : ('reg function_def) list;
  }

