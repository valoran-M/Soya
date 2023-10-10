type ident = string
type label = string
type reg = Mips.register

type instruction =
  | LLabel    of label
  | LPutchar  of reg
  | LMove     of reg * reg
  | LLoad     of Rtl.address * reg
  | LStore    of Rtl.address * reg
  | LPush     of reg
  | LOp       of Rtl.operation * reg list * reg
  | LCond     of Rtl.condition * reg list *  label
  | LCall     of ident
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
  functions : function_def list;
}