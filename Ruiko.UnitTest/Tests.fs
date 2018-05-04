module Tests

open System
open Xunit
open AST
open Parser
open System.Text.RegularExpressions

let inline (&=) (a: 'T) (b: 'G)  = obj.ReferenceEquals(a, b)


[<Fact>]
let ``Name Test`` () =

    let L1 = Literal(C"123")

    let L2 = Literal(R(Regex "(a)+"))

    let And = And([|Atom <| Lit L1; Atom <| Lit L2 |])

    Assert.Equal(And |> Parser''.ToName |> string, "C'123' R'(a)+'");

    let Or = Or([|And; And|]);



    
    result.finished = result.finished
    |>  Assert.True

    Assert.Equal(Or |> Parser''.ToName |> string, "C'123' R'(a)+' | C'123' R'(a)+'");


