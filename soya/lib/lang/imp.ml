type binop =
  | Add | Sub | Mul
  | Lt  | Le  | Ge | Gt
  | Eq  | Neq
  | And | Or

type unop = Neg | Not

type expression =
  | Char  of int 
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of unop   * expression
  | Binop of binop  * expression * expression
  | Call  of string * expression list
  | DCall of expression * expression list
  | Deref of expression * Op.mem_size
  | Alloc of expression
  | Addr  of string

type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  | Write   of expression * Op.mem_size * expression


and sequence = instruction list

type function_def = {
    name: string;
    params: string list;
    locals: string list;
    code: sequence;
  }

type program = {
    globals: string list;
    static : Op.static list;
    functions: function_def list;
  }

