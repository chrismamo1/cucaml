# TinyLisp

TinyLisp is a minimal Lisp-like language which is compiled into CUDA PTX code by CuCaml. It is somewhat similar the OCaml's untyped lambda format, which lowers the barrier to adapting CuCaml to run on OCaml code.

# Embedding TinyLisp in OCaml programs

Do it like this:

```ocaml
let src = {cucamlcl|
    some TinyLisp code
  |cucamlcl}
in
...
```

and then compile with a line like this in the OCamlBuild `_tags` file:

```
<myFile.{cmo,cmx}>: package(cucaml.ppx)
```

and `cucaml.ppx` will compile the TinyLisp source string into PTX code. This means that you don't need to pull in the entire CuCaml compiler as a runtime dependency for your project.

# The language

## Conditionals

Most TinyLisp constructs are treated as expressions, rather than statements, including the ubiquitous `if`. These are most of the boolean operations supported by TinyLisp:

| Name in source | Notes  |
| -------------- | ------ |
| `a > b`        |        |
| `a >= b`       |        |
| `a = b`        |        |
| `a <> b`       | negative equals |
| `not a`        |        |

For example, the following expression evaluates to `1`:

```
(if (= 2 2) 1 2)
```

## Variables and Functions

TinyLisp does not have pervasively inferred types (some static analysis is done to determine the most efficient representation of values), nor is it dynamically typed. When creating a CuCaml function (example below), variables should be declared, as should parameters.

Example, a trivial function to cube a floating-point number:

```
(fun cube ((float x))
  (let
    (float x2)
    (* x x)
    (* x2 x)))
```

Less trivial example: a kernel to capitalize every letter in a string

```
(fun isAlpha ((char c))
  (or
    (and
      (>= c 'A')
      (<= c 'Z'))
    (and
      (>= c 'a')
      (<= c 'z'))))

(fun isLower ((char x))
  (and (>= c 'a') (<= c 'z')))

(fun kernel ((char x))
  (if (isAlpha x)
    (let
      (char ulDiff)
      (- 'a' 'A')
      (if (isLower x)
        (- x ulDiff)
        x))
    x))
```

```
(fun abs ((float x))
  (if (< x 0.0)
    (- 0.0 x)
    x))

(fun kernel ((float x))
  (float guess)  (float step)
  (set step (set guess (/ x 2.0)))
  (while (> 0.001 (abs (- x (* guess guess))))
    (if (> (* guess guess) x)
      (set guess (- guess (set step (/ step 2.0))))
      (set guess (+ guess (set step (/ step 2.0))))))
  guess)
```

```
(fun isAlpha ((char c))
  (or
    (and
      (>= c 'A')
      (<= c 'Z'))
    (and
      (>= c 'a')
      (<= c 'z'))))

(fun isLower ((char x))
  (and (>= c 'a') (<= c 'z')))

(fun kernel ((char x))
  (if (isAlpha x)
    (let
      (char ulDiff)
      (- 'a' 'A')
      (if (isLower x)
        (- x ulDiff)
        x))
x))
```