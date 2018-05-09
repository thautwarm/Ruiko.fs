// Learn more about F# at http://fsharp.org

open System
open ParserC
open Tokenizer
open System.Text.RegularExpressions
open Utils
open Bootstrap
open System.Linq
open EDSL


[<EntryPoint>]
let main argv =
    
    let lang = LanguageArea()
    let (:=) = def'language lang


    let X, strings = 
            "X" := [
                     And[Ref "X"; L"c"];
                     And[L"123"; L"234"]
                   ]
    
    let strings = strings |> List.sort |> List.rev
    let space_lexer = Lexer [R <| Regex "\G\s"]
    
    let token'table =  
        List.map Literal.L strings 
        |> Lexer 
        |> fun it -> [("const_str", it.lex); ("space", space_lexer.lex)]
    
    let tokens = "
    123
    234
    c
        c
    c
    "
                |> Lexing (Map[]) token'table "testfile"
                |> Seq.filter (fun it -> it.name <> "space")
                |> Array.ofSeq


    tokens.ToList().ForEach(fun it -> printfn "%s" <| it.ToString())
    printf "%s" <| X.ToString()

    let state = State.New(lang)
    X |> ParserC.Match state tokens
    |> fun it ->
        printf "%s" <| it.ToString()
        match it with
        | Matched it ->
              printfn "%s" <| it.ToString()
        | _ -> ()
    



    
//    let lang = LanguageArea()
//    let state = State.New(lang)
    
//    let L_ = L >> Lit >> Atom
//    let R_ = Regex >> R >> Lit >> Atom
//    let N_ = N >> Lit >> Atom


//    let And_ = And >> Composed
//    let Or_  = Or  >> Composed

    
//    let when_ = When()
//    let with_ = With()
//    let refactor = None


//    let Named = (fun name  -> Named(name, when_, with_, refactor)) >> Atom

//    let X = "X" |> Const'Cast

    
//    lang.[X] <-  
//            Or_ [
//                And_ [Named X; L_ "c"]  ;
//                And_ [L_ "123"; R_"\G\d"] 
//               ] 
    
    

//    let space_lexer = Lexer [R <| Regex "\G\s"]

//    let token'table = Lexer [L "123"; L "c"; R (Regex "\G\d")]
//                     |> fun it -> [("marisa", it.lex); ("space", space_lexer.lex)]
        
    
//    let tokens = "
//    123 
//    5 
//        c 
//    c 
//    c   
//c
//    123 
//        123

//123
    
//    " 
//                  |> IndentedLexing (Map[]) token'table "testfile"
//                  |> Seq.filter (fun it -> it.name <> "space")
//                  |> Array.ofSeq
    
    
           
//    tokens.ToList().ForEach(fun it -> printfn "%s" <| it.ToString())

    

    //(Named X) |> ParserC.Match state tokens
    //|> fun it ->
    //    match it with
    //    | Matched it ->
    //          printfn "%s" <| it.ToString()
    //    | _ -> ()
   
    
    0 // return an integer exit code
