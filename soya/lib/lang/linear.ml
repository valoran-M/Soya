open Op

type label = string
type reg = Mips.register

type instruction =
  | LLabel    of label
  | LPutchar  of reg
  | LAlloc    of reg
  | LMove     of reg * reg
  | LLoad     of reg address * mem_size * reg
  | LStore    of reg address * mem_size * reg
  | LPush     of reg
  | LOp       of operation * reg list * reg
  | LCond     of condition * reg list *  label
  | LCall     of reg address
  | LGoto     of label
  | LReturn

type code = instruction list

type function_def = {
  stack_size  : int;
  name        : string;
  code        : code;
}

type program = {
  globals   : string list;
  static    : Op.static list;
  functions : function_def list;
}
