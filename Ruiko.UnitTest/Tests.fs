module Tests

open System
open Xunit
open AST
open Tokenizer
open Parser
open System.Text.RegularExpressions
open System.IO
open Utils

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
    let X = Named("X", When(), With(), lang, None)
    
    let tokens = [|{name="const"; value="a"; colno=1; lineno=0; filename=""};{name="const"; value="5"; colno=2; lineno=0; filename = ""}|]

    let state = State.New()
    let ast = X |> Parser''.Match tokens state lang
    File.AppendAllText("./log", ast.ToString())
    0


[<Fact>]
let ``tokenizer test``() = 
    let m_str = "
    I am the bone of my sword
    "

    let name_lexer = Lexer([R(Regex "\G[a-zA-Z_]{1}[a-zA-Z0-9_]*")])
   
    let space_lexer = Lexer([R(Regex "\G\s")])
    let TokenTable = [("Name",  name_lexer.lex);
                      ("Space", space_lexer.lex)]
                      |> List.map (fun (a, b) -> a|> Const'Cast, b)
    Lexing (Map[]) TokenTable m_str "test"
    |> List.ofSeq
    |> fun it ->  File.AppendAllText("./log", it.ToString())
    0