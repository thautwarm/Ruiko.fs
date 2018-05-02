module AST
open Tokenizer
open Utils

type AST = 
    | Single of Tokenizer 
    | Named  of string * AST
    | Nested of AST CList
    | Nil 
    with 
    member this.Name: string = 
        match this with
        | Single tk      -> tk.name
        | Named(name, _) -> name
        | Nested lst     -> lst |> Seq.map (fun it -> it.Name) |> String.concat " " |> sprintf "[%s]"

type AST'Help = 
    static member take'nested (ast: AST) =
        match ast with 
        | Nested lst -> Some(lst)
        | _          -> None
   
        

