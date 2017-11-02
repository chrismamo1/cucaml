
(* The type of tokens. *)

type token = 
  | SYM of (string)
  | RPAREN
  | NUM of (float)
  | LPAREN
  | EOF
  | CHAR of (char)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (BasicSExp.t option)
