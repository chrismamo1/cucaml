%token <float> NUM
%token <char> CHAR
%token LPAREN
%token RPAREN
%token <string> SYM
%token EOF
%start <BasicSExp.t option> prog
%%
prog:
  | EOF { None }
  | v = value { Some v }
  ;

value:
  | LPAREN; els = sexp_children; RPAREN
    { `SExp(els) }
  | n = NUM
    { `Num(n) }
  | c = CHAR
    { `Char(c) }
  | s = SYM
    { `Symb(s) }
  ;

sexp_children:
  | { [] }
  | kids = sexp_children; v = value
    { List.append kids [v] }
  ;