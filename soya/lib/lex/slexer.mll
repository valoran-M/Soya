{

  open Lexing
  open Sparser

    let keyword_or_ident =
    let h = Hashtbl.create 22 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "putchar",    PUTCHAR;
        "abstract",   ABSTRACT;
        "if",         IF;
        "else",       ELSE;
        "while",      WHILE;
        "true",       BOOL true;
        "false",      BOOL false;
        "var",        VAR;
        "function",   FUNCTION;
        "class",      CLASS;
        "attribute",  ATTRIBUTE;
        "static",     STATIC;
        "method",     METHOD;
        "this",       THIS;
        "super",      SUPER;
        "extends",    EXTENDS;
        "new",        NEW;
        "return",     RETURN;
        "char",       TYP_CHAR;
        "int",        TYP_INT;
        "bool",       TYP_BOOL;
        "void",       TYP_VOID;
        "instanceof", INSTANCEOF;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']                { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+      { token lexbuf }
  | "//" [^ '\n']* "\n"   { new_line lexbuf; token lexbuf }
  | "/*"                  { comment lexbuf; token lexbuf }
  | number as n           { CST(int_of_string n) }
  | ident as id           { keyword_or_ident id }
  | "'" _ as c "'"        { CHAR (c.[1]) }
  | ";"   { SEMI }
  | "="   { SET }
  | "!"   { NOT }
  | "&&"  { AND }
  | "||"  { OR  }
  | "+"   { PLUS }
  | "-"   { SUB  }
  | "*"   { STAR }
  | "<"   { LT }
  | "<="  { LE }
  | ">"   { GT }
  | ">="  { GE }
  | "=="  { EQ }
  | "!="  { NEQ }
  | "("   { LPAR }
  | ")"   { RPAR }
  | "{"   { BEGIN }
  | "}"   { END }
  | "["   { LBRACKET }
  | "]"   { RBRACKET }
  | "."   { DOT }
  | ","   { COMMA }
  | _     { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof   { EOF }

and comment = parse
  | "*/"    { () }
  | _       { comment lexbuf }
  | eof     { failwith "unfinished comment" }

