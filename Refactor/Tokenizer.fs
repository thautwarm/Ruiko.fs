module Tokenizer

open Utils
open System.Text.RegularExpressions
open Ruikowa.CSharp

type Tokenizer = {
    name     : string
    value    : string
    colno    : int
    lineno   : int
    filename : string //Maybe Path Object Sooner.
    }
    with 
        override this.ToString() = 
            sprintf "{name: %s, value: '%s', colno: %d, lineno: %d}" this.name this.value this.colno this.lineno

type Literal = 
    | R      of Regex
    | R'     of Regex
    
    | N      of string // Name
    | N'     of string
    
    
    | L      of string // Runtime String 
    | L'     of string

    | C      of string // Const String
    | C'     of string 


    | NC     of string * string
    | Fn     of (Tokenizer -> bool)




type Lexer = Lexer of Literal list
    with 
        member this.lex (raw: string) (pos: int): string option = 
            let rec processing': Literal list -> string option =
                function
                | []     -> None
                | x::xs  -> 
                    match x with
                    | R  regex
                    | R' regex ->
                        let r = regex.Match(raw, pos)
                        if r.Success then r.Value |> Some
                        else processing' xs
                
                    | C  literal 
                    | C' literal
                    | L  literal 
                    | L' literal ->
                        if StrUtils.StartsWithAt(raw, literal, pos)
                        then literal |> Some
                        else processing' xs
                    | otherwise  ->
                        sprintf "This literal spec(%s) cannot be used for constructing a lexer." (otherwise.ToString())
                        |> failwith

            match this with 
            | Lexer lst ->
                processing' lst
 
   
let Lexing (castMap: Map<string, string>) // the values of castMap must be const strings in Utils.ConstStrPool.
           (tokenTable: ((string * (string -> int -> string option)) list)) 
           (filename: string)
           (raw: string)
          
          : Tokenizer seq =
    if raw.Length = 0 then seq []
    else 
    let n = raw.Length

    let rec processing' (lineno, colno, pos) 
                        (tokenTable': (string * (string -> int -> string option)) list) = seq{
        
        match tokenTable' with 
        | []     ->  
            System.Diagnostics.Trace.TraceWarning("No lexer defined for processing '%s'", raw.[pos].ToString() |> (Regex.Escape))
            
            let lineno', colno' =
                if raw.[pos] = '\n' then 
                    (lineno + 1, 0)
                 else 
                    (lineno, colno + 1)
            
            let pos' = pos + 1
            if n  = pos' then ()
            else yield! processing' (lineno', colno', pos') tokenTable
            
        | (name, x) :: xs ->
            
            match x raw pos with 
            | None   -> 
                yield! (processing' (lineno, colno, pos) xs) 
            | Some r ->
                
                match castMap.TryFind r with
                | None ->
                    yield {value = r; name=name; colno = colno; lineno = lineno; filename=filename}
                | Some casted'name ->
                    yield {value = r |> Const'Cast; name = casted'name; colno = colno; lineno = lineno; filename = filename}

                let row_inc = r |> Seq.where (fun it -> it = '\n') |> Seq.length
                let inc = r.Length

                let lineno', colno' = 
                    if row_inc <> 0 then
                        r
                        |> Seq.findIndexBack (fun it -> it = '\n') 
                        |> fun it -> 
                            (lineno + row_inc, inc - it - 1)
                    else
                        (lineno, colno + inc)

                let pos' = pos + inc
                if n  = pos' then ()
                else yield! (processing' (lineno', colno', pos') tokenTable)
     }

    processing' (0, 0, 0) tokenTable


        


      
    
    
    
    