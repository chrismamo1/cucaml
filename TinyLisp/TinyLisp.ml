open TinyLispLexer

type literal =
  | Float of float
  | Int of int
  | Char of char

type t =
  | SExp of t list
  | Symb of string
  | Literal of literal
  | Func of string * string list * t
  | FCall of string * t list
  (*| Return of t*)
  | IfElse of t * t * t
  (* refers to the `let` binding of a name to a value
     and the expressions which subsequently see this  *)
  | Binding of string * t * t
let rec parse_and_print lexbuf =
  match TinyLispParser.prog TinyLispLexer.read lexbuf with
    | Some(x) -> print_endline (BasicSExp.pprint x)
    | None -> print_endline "We're fucked :'("

let rec build source =
  let lexbuf = Lexing.from_string source in
  let basic =
    match TinyLispParser.prog TinyLispLexer.read lexbuf with
      | Some(x) -> x
      | None -> raise(Failure "Invalid TinyLisp syntax")
  in
  let open BasicSExp in
  let rec aux ?(need=true) (x: BasicSExp.t) =
    match x with
    | `SExp(
            (`Symb "fun")
          ::(`Symb name)
          ::(`SExp params)
          ::body
          ::[]) ->
        let params =
          List.map
            (function
              | `Symb x -> x
              | _ -> raise (Failure "compound expressions aren't valid parameter names"))
            params
        in
        Func(name, params, aux body)
    | `SExp(
            (`Symb "let")
          ::(`Symb name)
          ::value
          ::body
          ::[]) ->
        Binding(name, aux ~need:true value, aux body)
    | `SExp(
          (`Symb "if")
          ::cond
          ::thn
          ::els
          ::[]) ->
        let need = true in
        IfElse(aux ~need cond, aux ~need thn, aux ~need els)
    (*| `SExp(
          (`Symb "return")
        ::body
        ::[]) ->
      Return(aux ~need:true body)*)
    | `SExp(`Symb name :: args) when need = true ->
        FCall(name, List.map (aux ~need) args)
    | `SExp((`Symb _ :: _) as kids) when need = false ->
        SExp(List.map (aux ~need) kids)
    | `Num(n) ->
        Literal(Float(n))
    | `Char(c) ->
        Literal(Char(c))
    | `Symb(s) -> Symb(s)
    | `SExp(ls) ->
        SExp (List.map (aux ~need) ls)
  in
  aux basic

(*let () =
  let testCode = {|
    (lambda f (x) (print x))
  |}
  in
  let _ = Printf.printf "Trying to parse the string %s\n" testCode in
  let _ = parse_and_print (Lexing.from_string testCode) in
  ()*)