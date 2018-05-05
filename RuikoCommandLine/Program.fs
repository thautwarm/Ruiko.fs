// Learn more about F# at http://fsharp.org

open System
open AST
open Tokenizer
open Parser
open System.Text.RegularExpressions
open System.IO

let inline (&=) (a: 'T) (b: 'G)  = obj.ReferenceEquals(a, b)

[<EntryPoint>]
let main argv =
    let L1 = Literal(L"a")

    let L2 = Literal(L"b")

    let And = And([Atom <| Lit L1; Atom <| Lit L2 ])

    let Or = Or([And; And]);



    let lang = LanguageArea()
    lang.Add ("X", Or)
    let X = Named("X", When(), With(), lang)
    
    let tokens = [|{name="const"; value="a"; colno=1; lineno=0;};{name="const"; value="b"; colno=2; lineno=0;}|]

    let state = State.New()
    let ast = X |> Parser''.Match tokens state lang
    
    ast.ToString() |> printfn "%s" 
    0
