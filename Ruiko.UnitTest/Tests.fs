module Tests

open System
open Xunit
open ParserC
open Tokenizer
open System.Text.RegularExpressions
open Utils

[<Fact>]
let ``left reursion`` () =
    
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
        
    
    let tokens = "123 5 c c c" 
                  |> Lexing (Map[]) token'table "testfile"
                  |> Seq.filter (fun it -> it.name <> "space")
                  |> Array.ofSeq
    
                
                 
    let ast = 
        match (Named X) |> ParserC.Match state tokens with 
        | Matched ast ->
            ast 
        | _           -> Nil
    let rec check(ast) =
        match ast with 
        | Nil -> false
        | Node(name, Nested astLst) ->
            if name <> X then false
            else check <| Seq.head astLst 
        | _  ->
            true
       
    Assert.True(check ast)


[<Fact>]
let ``test indented lexer``() = 
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

    
    Assert.True(
        tokens.[0].name &= SYMBOL.INDENT, tokens.[0].ToString())
    
    Assert.True(
        tokens.[3].name &= SYMBOL.INDENT, "2")
    
    Assert.True(
        tokens.[5].name &= SYMBOL.DEDENT, "3")
    
    Assert.True(
        tokens.[8].name &= SYMBOL.DEDENT, tokens.[7].ToString())
    Assert.True(
        tokens.[10].name &= SYMBOL.INDENT, "5")
    Assert.True(
        tokens.[14].name &= SYMBOL.DEDENT && tokens.[15].name &= SYMBOL.DEDENT, "continous dedent")
