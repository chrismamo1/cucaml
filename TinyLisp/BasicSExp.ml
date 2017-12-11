type t =
  [ `SExp of t list
  | `Symb of string
  | `Floatlit of float
  | `Intlit of int
  | `Char of char
  ]

let rec pprint = function
  | `SExp(ls) ->
      let rec aux = function
        | hd :: tl ->
            pprint hd ^ "\t" ^ aux tl
        | [] -> ""
      in
      "(" ^ aux ls ^ ")"
  | `Symb(s) -> s
  | `Floatlit(f) -> string_of_float f
  | `Intlit(n) -> string_of_int n
  | `Char(c) -> "'" ^ Char.escaped c ^ "'"