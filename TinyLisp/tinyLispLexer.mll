{
  open Lexing
  open TinyLispParser

  let parse_error x =
    Printf.printf "Custom parse_error: %s\n\n" x
}

let lparen = '('
let rparen = ')'
let sym = ['a'-'z' 'A'-'Z' '_' '=' '>' '<' '*' '/' '+' '-'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '/' '+' '-' '.' '=' '>' '<']*
let white = [' ' '\t' '\n' '\r']+
let real = ['+' '-']? ('.' ['0'-'9']+ | ['0'-'9']+ '.' ['0'-'9']+ | ['0'-'9']+ '.')
let int = ['+' '-']? ['0'-'9']+
let halfq = '\''
let halfq_e = "\\'"
let bs_e = "\\\\"
let litchar_code = '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
let litchar = halfq_e | litchar_code | ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '/' '%' '[' ']' '{' '}' '"' '(' ')' '*' '&' '^' '$' '#' '@' '!' '`' '~' ' ' '+' '=' '<' '>' '.' ',' '?']

rule read =
  parse
  | white { read lexbuf }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | halfq halfq_e halfq { CHAR('\'') }
  | halfq bs_e halfq { CHAR('\\') }
  | halfq (litchar_code as lcc) halfq {
        let tl = String.sub lcc 1 3 in
        let code0 = Char.code '0' in
        let code =
            ((Char.code tl.[0] - code0) * 64)
          + ((Char.code tl.[1] - code0) * 8)
          + (Char.code tl.[2] - code0)
        in
        CHAR(Char.chr code)
      }
  | halfq (litchar as lc) halfq {
        CHAR(lc.[0])
      }
  | sym { SYM(Lexing.lexeme lexbuf) }
  | real { FLOATLIT(float_of_string (Lexing.lexeme lexbuf)) }
  | int { INTLIT(int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }