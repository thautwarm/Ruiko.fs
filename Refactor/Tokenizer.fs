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


let ``INDENT Symbol`` = "INDENT" |> Const'Cast;
let ``DEDENT Symbol`` = "DEDENT" |> Const'Cast;
let Space = Regex "\G\s+"


let BeIndent (lineno: int) (colno: int) (filename: string) = 
    {value=System.String.Empty; name=``INDENT Symbol``; colno=colno; lineno=lineno; filename=filename}

let BeDeDent (lineno: int) (colno: int) (filename: string) = 
    {value=System.String.Empty; name=``DEDENT Symbol``; colno=colno; lineno=lineno; filename=filename}

let update'cursor (lineno, colno) (r: string) = 
    let row_inc = r |> Seq.where (fun it -> it = '\n') |> Seq.length
    let inc = r.Length
    if row_inc <> 0 then
        r
        |> Seq.findIndexBack (fun it -> it = '\n') 
        |> fun it -> 
            (lineno + row_inc, inc - it - 1)
    else
        (lineno, colno + inc)
    
let IndentedLexing 
           (castMap: Map<string, string>) // the values of castMap must be const strings in Utils.ConstStrPool.
           (tokenTable: ((string * (string -> int -> string option)) list)) 
           (filename: string)
           (raw: string)
          
          : Tokenizer seq =
    if raw.Length = 0 then seq []
    else 
    let n = raw.Length
    let mutable indent'level = CStack [0]
    let mutable new'line = true
    let mutable lineno = 0
    let mutable colno = 0
    let mutable pos = 0

    let rec processing' (tokenTable': (string * (string -> int -> string option)) list) = seq{

        // indent/dedent
        if new'line then 
            new'line <- false
            let r = Space.Match(raw, pos)
            if r.Success then 
                r.Value
                |>
                function
                | str ->
                    pos <- pos + str.Length

                    str
                    |> Seq.tryFindIndexBack (fun it -> it = '\n')
                    |> 
                    function
                    | None     -> 
                        colno  <- str.Length
                            
                    | Some idx -> 
                        let l, c = update'cursor(lineno, colno) str
                        lineno <- l
                        colno  <- c
                
                let mutable current = indent'level |> Seq.head 
                while current <> colno do 
                        
                    yield 
                        if colno > current then 
                            BeIndent  >> bye'with (fun () -> indent'level.Push colno) 
                        else BeDeDent >> bye'with (fun () -> current <- indent'level.Pop())
                        <| lineno <| colno <| filename
                    
                    current <- indent'level |> Seq.head 
                    
            else 
                ()
        else 
            ()
        
        if n = pos then ()
        else 

        match tokenTable' with 
        | []     ->  

            System.Diagnostics.Trace.TraceWarning(
                "No lexer defined for processing '%s'", raw.[pos].ToString() |> (Regex.Escape))
            
            if raw.[pos] = '\n' then 
                new'line <- true
                lineno <- lineno + 1
                colno  <- 0
            else
                colno <- colno + 1
            
            pos <- pos + 1

            if n  = pos then ()
            else yield! processing' tokenTable
            
        | (name, x) :: xs ->
            

            match x raw pos with 
            | None   -> 
                yield! processing' xs 

            | Some r ->
                
                match castMap.TryFind r with
                | None ->
                    yield {value = r; name=name; colno = colno; lineno = lineno; filename=filename}
                | Some casted'name ->
                    yield {value = r |> Const'Cast; name = casted'name; colno = colno; lineno = lineno; filename = filename}


                let lineno', colno' = update'cursor (lineno, colno) r
                lineno <- lineno'
                colno  <- colno'
                
                pos <- pos + r.Length
                if n  = pos then ()
                else 
                if StrUtils.StartsWithAt(raw, "\n", pos)
                then
                    new'line <- true
                else 
                    ()
                
                yield! processing' tokenTable
     }

    processing' tokenTable



        


      
    
    
    
    