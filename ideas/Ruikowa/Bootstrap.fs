(** 
The implementation of Ruiko lang.
**)
module Bootstrap

open Parser
open AST 
open Tokenizer
open Utils
open System.Text.RegularExpressions

// Str := R'<string>';


let Ruiko'Str'Lexer = "\G'([^\']+|\.)*?'|\G'([^\']+|\.)*?'"
                      |> Regex |> RegExp |> fun it -> [it]
                      |> Lexer


let Ruiko'Name'Lexer = "\G[a-zA-Z_]{1}[a-zA-Z_0-9]*"
                      |> Regex |> RegExp |> fun it -> [it]
                      |> Lexer


let Ruiko'Keyword = ["cast"; "as"; "at"; "when"; "with"; "import"; "where"]
                    </> Const'Cast
                    //|> Lexer 

let Ruiko'Name = "Name"
                 |> Const'Cast |> Name |> Literal

let Ruiko'Str =  "Str" 
                 |> Const'Cast |> Name |> Literal

let Ruiko'Keyword'Throw = "throw" 
                          |> Const'Cast |> ConstStr |> Literal

let Ruiko'Keyword'Ignore = "ignore"
                           |> Const'Cast |> ConstStr |> Literal

                  
                  


let Ruiko'CastMap  = 
    Seq.replicate (Ruiko'Keyword.Length) ("keyword" |> Const'Cast)
    |> Seq.zip Ruiko'Keyword
    |> Map.ofSeq



let Ruiko'Stmts = 
    [Atom]
                        





// Name    := 
// Keyword := 




