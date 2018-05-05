module Tests

open System
open Xunit
open AST
open Tokenizer
open Parser
open System.Text.RegularExpressions
open System.IO

let inline (&=) (a: 'T) (b: 'G)  = obj.ReferenceEquals(a, b)


[<Fact>]
let ``Name Test`` () =


    let L1 = Literal(L"a")

    let L2 = Literal(R(Regex "\d"))

    let And = And([Atom <| Lit L1; Atom <| Lit L2 ])

    Assert.Equal(And |> Parser''.ToName |> string, "L'a' R'\d'");

    let Or = Or([And; And]);



    Assert.Equal(Or |> Parser''.ToName |> string, "L'a' R'\d' | L'a' R'\d'");

    let lang = LanguageArea()
    lang.Add ("X", Or)
    let X = Named("X", When(), With(), lang)
    
    let tokens = [|{name="const"; value="a"; colno=1; lineno=0;};{name="const"; value="5"; colno=2; lineno=0;}|]

    let state = State.New()
    let ast = X |> Parser''.Match tokens state lang
    File.WriteAllText("./log", ast.ToString())
    0


