open TinyLispLexer

type literal =
  | Float of float
  | Int of int
  | Char of char

type t =
  | Assign of string * t
  | SExp of t list
  | Symb of string
  | Literal of literal
  | Func of string * t list * t list
  | FCall of string * t list
  (*| Return of t*)
  | IfElse of t * t * t
  (* refers to the `let` binding of a name to a type, a value,
     and the expressions which subsequently see this  *)
  | Binding of string * string * t * t
  | Declaration of string * string
  | While of t * t list
let rec parse_and_print lexbuf =
  match TinyLispParser.prog TinyLispLexer.read lexbuf with
    | xs -> List.iter (fun x -> print_endline (BasicSExp.pprint x)) xs
    | [] -> print_endline "We're fucked :'("

let rec build source =
  let lexbuf = Lexing.from_string source in
  let basic =
    match TinyLispParser.prog TinyLispLexer.read lexbuf with
      | [] -> raise(Failure "Invalid TinyLisp syntax (or empty?)")
      | xs -> xs
  in
  let open BasicSExp in
  let rec aux ?(need=true) (x: BasicSExp.t) =
    match x with
    | `SExp(
            (`Symb "fun")
          ::(`Symb name)
          ::(`SExp params)
          ::body ) ->
        let params =
          List.map
            (fun sexp ->
              aux ~need:true sexp
              |> function
              | (Declaration _) as x -> x
              | _ -> raise (Failure("parameter lists should be lists of declarations")))
            params
        in
        Func(name, params, List.map (aux ~need:true) body)
    | `SExp(
            (`Symb (("float" | "int" | "bool" | "char") as typeName))
          ::(`Symb name)
          ::[]) ->
        Declaration(name, typeName)
    | `SExp(
            `Symb "let"
          ::(`SExp (`Symb(("float" | "int" | "bool" | "char") as typeName) :: `Symb(name) :: []))
          ::value
          ::body
          ::[]) ->
        Binding(name, typeName, aux ~need:true value, aux body)
    | `SExp(
            `Symb "set"
          ::`Symb name
          ::value
          ::[]) ->
        Assign(name, aux ~need:true value)
    | `SExp(
            `Symb "while"
          ::cond
          ::body) ->
        While(aux ~need:true cond, List.map (aux ~need:true) body)
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
    | `Floatlit(f) ->
        Literal(Float(f))
    | `Intlit(n) ->
        Literal(Int(n))
    | `Char(c) ->
        Literal(Char(c))
    | `Symb(s) -> Symb(s)
    | `SExp(ls) ->
        SExp (List.map (aux ~need) ls)
  in
  List.map aux basic

(*let () =
  let testCode = {|
    (lambda f (x) (print x))
  |}
  in
  let _ = Printf.printf "Trying to parse the string %s\n" testCode in
  let _ = parse_and_print (Lexing.from_string testCode) in
  ()*)