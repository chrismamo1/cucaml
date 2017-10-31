type t =
  [ `SExp of t list
  | `Symb of string
  | `Num of float
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
  | `Num(n) -> string_of_float n
  | `Char(c) -> "'" ^ Char.escaped c ^ "'"