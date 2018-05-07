// Learn more about F# at http://fsharp.org

open System
open AST
open Tokenizer
open Parser
open Utils
open System.Text.RegularExpressions
open System.IO

let inline (&=) (a: 'T) (b: 'G)  = obj.ReferenceEquals(a, b)


[<EntryPoint>]
let main argv =

    let L1 = Literal(ValueStr"a")
    let L2 = Literal(RegExp(Regex "\G\d"))
    let L3 = Literal(RegExp(Regex "\G[A-Z]"))

    let and1 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L2 ])
    let and2 = And([Atom <| ``Literal Atom`` L1; Atom <| ``Literal Atom`` L3 ])

    let Or' = Or([and2; and1])

    printfn "%s  %d" (Or' |> Parser.name) (Or'.structure.Length)

    0
    
    

    //let m_str = "
    //I am the bone of my sword
    //"

    //let name_lexer = Lexer([RegExp(Regex "\G[a-zA-Z_]{1}[a-zA-Z0-9_]*")])
    //let space_lexer = Lexer([RegExp(Regex "\G\s")])
    //let TokenTable = [("Name",  name_lexer.lex);
    //                  ("Space", space_lexer.lex)]
    //                  |> List.map (fun (a, b) -> a|> Const'Cast, b)
    
    //let tokens = Lexing (Map[]) TokenTable m_str "test"
    


    //let L2 = Literal(L"b")

    //let L3 = Literal(L"c")

    //let atom1 = Atom <| Atom'Core.Binding ("elem", Atom <| Lit L3)

    //let lang = LanguageArea()

    //let try_name_binding = true

    //let X = 
    //    if try_name_binding then 
    //        fun (state: State) ->
    //            AST.Named("X", CList([|state.context.["elem"]|]))
    //        |> Some
    //    else 
    //        None
    //    |> fun it -> Named("X", When(), With(), lang, it)
        

    //let and1 = And([Atom <| Lit L1; Atom <| Lit L2 ])

    //let lr_and = And([Atom <| Atom'Core.Named X; atom1])

    //let or1 = Or([lr_and; and1]);

   
    //lang.Add ("X", or1)
    
    //let tokens = [|{name="const"; value="a"; colno=1; lineno=0; filename=""};
    //               {name="const"; value="b"; colno=2; lineno=0; filename=""};
    //               {name="const"; value="c"; colno=3; lineno=0; filename=""};
    //               {name="const"; value="c"; colno=4; lineno=0; filename=""};
    //               {name="const"; value="c"; colno=5; lineno=0; filename=""};
    //             |]

    //let state = State.New()
    //let ast = X |> Parser''.Match tokens state lang
    //ast.ToString() |> printfn "%s" 
    0
