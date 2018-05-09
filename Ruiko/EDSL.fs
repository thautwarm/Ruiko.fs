module EDSL
open ParserC
open Utils

type EDSL =
    | L of string 
    | N of string 

    | And of EDSL list
    | Or  of EDSL list
    | Seq of EDSL * int * int

    | Binding of string * EDSL

    | Ref     of string

let def (lang: LanguageArea) (name: string) (named'parser: Parser) =
    lang.[name|> Const'Cast] <- named'parser

    
    

   
