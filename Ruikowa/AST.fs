module AST
open Tokenizer
open Utils

type AST = 
    | Single of Tokenizer 
    | Named  of string * (AST CList)
    | Nested of AST CList
    | Nil 
    with 
    member this.Name: string = 
        match this with
        | Single tk      -> tk.name
        | Named(name, _) -> name
        | Nested lst     -> lst |> Seq.map (fun it -> it.Name) |> String.concat " " |> sprintf "[%s]"
        | Nil            -> failwith "Try to access the Name of Empty AST"

type AST'Help = 
    static member take'nested (ast: AST) =
        match ast with 
        | Nested lst -> Some(lst)
        | _          -> None
   
        

