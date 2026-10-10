/// Rows can be recursive: an expression type needs no declaration.
module Samples.Recursive

let rec eval expr =
    match expr with
    | `Num n -> n
    | `Add(a, b) -> eval a + eval b
    | `Neg e -> -(eval e)

/// The same type, written out: 'e stands for the type itself.
type Expr = ([ `Num of int | `Add of 'e * 'e | `Neg of 'e ] as 'e)

let rec simplify (expr: Expr) : Expr =
    match expr with
    | `Neg(`Neg e) -> simplify e
    | `Neg e -> `Neg(simplify e)
    | `Add(a, b) -> `Add(simplify a, simplify b)
    | `Num _ -> expr

let example = eval (simplify (`Add(`Num 2, `Neg(`Neg(`Num 5)))))
