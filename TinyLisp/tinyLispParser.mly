%token <float> FLOATLIT
%token <int> INTLIT
%token <char> CHAR
%token LPAREN
%token RPAREN
%token <string> SYM
%token EOF
%start <BasicSExp.t list> prog
%%
prog:
  | EOF { [] }
  | v = value; EOF
    { [v] }
  | v1 = value; v2 = value; EOF
    { [v1; v2] }
  | v1 = value; v2 = value; v3 = value; EOF
    { [v1; v2; v3] }
  | v = value; vs = prog
    { v :: vs }
  ;

value:
  | LPAREN; els = sexp_children; RPAREN
    { `SExp(els) }
  | f = FLOATLIT
    { `Floatlit(f) }
  | n = INTLIT
    { `Intlit(n) }
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