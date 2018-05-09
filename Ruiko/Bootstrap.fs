module Bootstrap

open ParserC
open Tokenizer
open EDSL
open System.Text.RegularExpressions
open Utils

let rec to'parser (ors: EDSL list) : Parser list * string list = 
    let rec trans (parser'lst: Parser list) (const'strings: string list) (ors': EDSL list) : Parser list * string list = 

        match ors' with
        | []    -> parser'lst, const'strings
        | x::xs -> 
            match x with 
            | N name ->
                let name' = name |> Const'Cast
                let p = Literal.N name' |> Lit |> Atom
                trans (p::parser'lst) (name'::const'strings) xs

            | L l ->
                l
                |> Const'Cast
                |> 
                function
                | it -> 
                    let p = Literal.L it |> Lit |> Atom 
                    trans (p::parser'lst) (it::const'strings) xs

            | And atoms ->
                let parser'list, string'list = to'parser atoms
                let and' = Composed.And (parser'list |> List.rev) |> Composed
                trans (and'::parser'lst) (string'list |> List.append const'strings) xs 
            
            | Or ands ->
                let parser'list, string'list = to'parser  ands
                let or' = Composed.Or (parser'list |> List.rev) |> Composed
                trans (or'::parser'lst) (string'list |> List.append const'strings) xs 
            
            | Binding(n, p) ->
                let n' = n |> Const'Cast
                let p'::p, c's = to'parser [p]
                trans (p'::parser'lst) (n'::(const'strings |> List.append c's)) xs
            | Ref n ->
                let n' = n |> Const'Cast
                let p = Named(n', When(), With(), None) |> Atom
                trans (p::parser'lst) (n'::const'strings) xs 
            | Seq _ ->
                raise NotImplemented
                
    trans [] [] ors 

let def'language (lang: LanguageArea) =
    let def' = def lang 
    fun (name:string) (definition: EDSL list) ->

        let a, b = to'parser definition

        let parser = 
            if List.length a |> (=) 1 
            then
                a |> List.head

            else 
                 Composed.Or <| List.rev a |> Composed

        def' name parser 
        parser, b |> List.distinct
        
      
     
        
        
        



        
    



