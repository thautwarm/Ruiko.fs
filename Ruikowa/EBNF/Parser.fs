module Parser3

open Ruikowa.CSharp
open RuikoCollections
open ConstStringPool
open Utils
open System.Text.RegularExpressions
open System

type Tokenizer = {
    name   : string
    value  : string
    colno  : int
    lineno : int
}

let inline (|StartWith|_|)(view: Tokenizer ArrayView) = 
    if view.start >= view.arr.Length then None
    else Some(view.arr.[view.start])


type  AST  = 
    | Token  of Tokenizer
    | Nested of AST CList
    | Named  of string * AST CList
    | Nil

and Status = Matched | Unmatched | Finsihed | LeftRecursive of string

and LanguageArea = CDict<string, Or>

and  Result(status: Status, ast:AST) = 
    member this.status = status 
    member this.ast    = ast
    static member finished            = Result(Finsihed , AST.Nil)
    static member unmatched           = Result(Unmatched, AST.Nil)
    static member matched (ast: AST)  = Result(Matched,   ast)

and  State  = {
    mutable count: int
    mutable trace: string Trace
    mutable context: Map<string, AST>
}




and Parser =
    abstract member Match: Tokenizer array -> State -> LanguageArea -> Result
    abstract member Name: string

and Literal =
    | R of Regex
    | N of string // Name
    | V of string // runtime string
    | C of string // const string
    interface Parser with 
        member this.Name: string =  raise (NotImplementedException())
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        

and Atom = 
    | Literal of Literal
    | Named   of string
    | Seq     of Or array * int * int
    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  raise (NotImplementedException())

and And = 
    | S of Atom array 
    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  raise (NotImplementedException())

and Or =
    | S of And array
    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  raise (NotImplementedException())

    
