// Learn more about F# at http://fsharp.org

open System
open ParserC
open Tokenizer
open System.Text.RegularExpressions
open Utils
open System.Linq


[<EntryPoint>]
let main argv =
        
    let lang = LanguageArea()
    let state = State.New(lang)
    
    let L_ = L>>Lit>>Atom
    let R_ = Regex >> R >> Lit >> Atom
    let And_ = And >> Composed
    let Or_  = Or  >> Composed
    let when_ = When()
    let with_ = With()
    let refactor = None


    let Named = (fun name  -> Named(name, when_, with_, refactor)) >> Atom
    let X = "X" |> Const'Cast

    
    lang.[X] <-  
            Or_ [
                And_ [Named X; L_ "c"]  ;
                And_ [L_ "123"; R_"\G\d"] 
               ] 
    
    

    let space_lexer = Lexer [R <| Regex "\G\s"]

    let token'table = Lexer [L "123"; L "c"; R (Regex "\G\d")]
                     |> fun it -> [("marisa", it.lex); ("space", space_lexer.lex)]
        
    
    let tokens = "
    123 
    5 
        c 
    c 
    c   
c
    123 
        123

123
    
    " 
                  |> IndentedLexing (Map[]) token'table "testfile"
                  |> Seq.filter (fun it -> it.name <> "space")
                  |> Array.ofSeq
    
    
           
    tokens.ToList().ForEach(fun it -> printfn "%s" <| it.ToString())

    

    //(Named X) |> ParserC.Match state tokens
    //|> fun it ->
    //    match it with
    //    | Matched it ->
    //          printfn "%s" <| it.ToString()
    //    | _ -> ()
   
    
    0 // return an integer exit code
