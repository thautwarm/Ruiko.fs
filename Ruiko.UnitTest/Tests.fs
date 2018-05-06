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


    let L1 = Literal(ValueStr"a")

    let L2 = Literal(RegExp(Regex "\G\d"))

    let L3 = Literal(RegExp(Regex "\G[a-z]{1}"))

    let and1 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L2 ])

    let and2 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L3 ])

    Assert.Equal(and1 |> Parser.name |> string, "L'a' R'\G\d'");

    let Or = Or([and1; and2]);


    Assert.Equal("L'a' (R'\G\d' | R'\G[a-z]{1}')", Or |> Parser.name |> string);

    let lang = LanguageArea()
    lang.Add ("X", Or)
    let X = Named("X", When(), With(), lang, None)
    
    let tokens = [|{name="const"; value="a"; colno=1; lineno=0; filename=""};{name="const"; value="5"; colno=2; lineno=0; filename = ""}|]

    let state = State.New()
    let ast = X |> Parser.match' tokens state lang
    File.AppendAllText("./log", ast.ToString())
    0


[<Fact>]
let ``tokenizer test``() = 
    let m_str = "
    I am the bone of my sword
    "

    let name_lexer = Lexer([RegExp(Regex "\G[a-zA-Z_]{1}[a-zA-Z0-9_]*")])
   
    let space_lexer = Lexer([RegExp(Regex "\G\s")])
    let TokenTable = [("Name",  name_lexer.lex);
                      ("Space", space_lexer.lex)]
                      |> List.map (fun (a, b) -> a|> Const'Cast, b)
    Lexing (Map[]) TokenTable m_str "test"
    |> Array.ofSeq
    |> fun it ->  File.AppendAllText("./log", it.ToString())
    0

[<Fact>]
let ``optimization test``() =
    let L1 = Literal(ValueStr"a")
    let L2 = Literal(RegExp(Regex "\G\d"))
    let L3 = Literal(RegExp(Regex "\G[A-Z]"))

    let and1 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L2 ])
    let and2 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L3 ])

    let Or' = Or [and2; and1] |> Parser.optimise
    Assert.Equal(1, Or'.structure |> List.length)

    
