# Soya language

Soya is an imperative object oriented language.

We have two type of structures :
- Primitive type
- Object

## Primitive type

### int

```
int x = 42;
```

### char

```
c := '_' | n
```

```
char c = 'c';
```

### bool

```
b := true | false
```

```
bool b = true;
```

## Object

You can declare an object with attributes methods and static methods.
An `a` object can inherits from another object `b`, this mean all attributes and
all methods of `b` are in `b`

An object can be abstract (we can not create an instance) this type of object
can declare abstract methods. If an `a` object inherits from an abstract `b`
object, the object `a` need to implement all abstract methods of `b`.

the rule `<name>` is a string, `list(<r>)` is a list of the rule `<r>`
and `sep_list('c', <r>)` is a list of the rule `<r>` separated by the character
`c`.

```
<class> :=
  | class <name> ?(extends <name>) {
        list(<attributes>)
        list(<methods>)
    }
  | abstract class <name> ?(extends <name>) {
        list(<attributes>)
        list(<abs_methods>)
    }

<attributes> :=
  attributes <type> <name>;

<methods> :=
  methods <ret_type> <name>(sep_list(',' <param>)) {
      <function_body>
  }

<abs_methods> := abstract <methods>

<param> := type <name>

<type> :=
  int | bool | char | <name>

<ret_type> :=
  <type> | void

```

Example:

```
class point {
  attribute int x;
  attribute int y;
  method void constructor(int x, int y) {
    this.x = x;
    this.y = y;
  }
  method int sum(int x) {
    return (x+this.x+this.y);
  }
}
```

## Imperative

### function

You can declared functions

```
function <type> <name> (sep_list(',' <name>)) {
  <function_body> 
}

<function_body> :=
  | var <type> <name>;
  | <type> <name> = <expr>;
  | list(<instruction>)
```

### Ast documented

```ocaml
type binop =
  | Add | Sub | Mul
  | Lt
  | And | Or

typ expr =
  | Char  of int                        (* 'c' *)
  | Cst   of int                        (* n *)
  | Bool  of bool                       (* true | false *)
  | Var   of string                     (* v *)
  | Binop of binop * expr * expr        (* e1 op e2 *)
  | Call  of string * expr list         (* f(e1, ..., en) *)
  | MCall of expr * string * expr list  (* c.f(e1, ..., en) *)
  | New   of string * expr list         (* new class(e1, ..., en) *)
  | NewTab     of typ * expr            (* new [<type>, i] *)
  | Instanceof of expr * string         (* c instance <name> *)
  | This                                (* this *)
  | Super                               (* super *)
  | Read  of mem
and  mem =
  | Arr of expr * exp     (* array access     e1[e2]  *)
  | Atr of expr * string  (* attribute access  o.x    *)

type instruction =
  | Putchar of expr                 (* puthcar(c) *)
  | Set     of string * expr        (* x = e *)
  | If      of expr * seq * seq     (* if (c) { et } else { ef } *)
  | While   of expr * seq           (* while (c) { e } *)
  | Return  of expr                 (* return e *)
  | Expr    of expr                 (* e *)
  | Write   of mem * expr           (*   m = e;   *)
and seq = instruction list
```

### variable

You need to declare all variable in the first function bloc (not in `if` and
`while` bloc).

```
var <type> <name>;
or
<type> <name> = <expr>;
```

## Cast object

You can change the static type of an object, under certain conditions :

- if `o2` inherits `o1`:

```
o1 _ = new o2();
```

- if you test if you object is an instance of the destination object.

Example (`c` an object):

```
var o1 c';
if (c instanceof o1) {
  c' = c
}
```
