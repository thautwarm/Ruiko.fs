module Tests

open System
open Xunit
open ParserC
open Tokenizer
open System.Text.RegularExpressions
open Utils

[<Fact>]
let ``My test`` () =
    
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
        
    
    let tokens = "123 c 4 5 6 888 c c 123 4 5" |> Lexing (Map[]) token'table "testfile"
                  |> Array.ofSeq
    
                
                 
    
    (Named X) |> ParserC.Match state tokens
    


    Assert.True(true)
