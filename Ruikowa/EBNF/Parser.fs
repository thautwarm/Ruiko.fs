module Parser
open Ruikowa.CSharp
open System.Text.RegularExpressions

type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
let ConstStrPool: CDict<string, string> = new CDict<string, string>()

let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (true, _) -> 
        ConstStrPool.Add(s, s)
        s
    | (false, r) -> r
    

type Tokenizer = {
    name   : string
    value  : string
    colno  : int
    lineno : int
}

type State = {
    mutable count: int
    mutable trace: Parser Trace
    mutable context: Map<string, AST>
}
and Parser = 
    abstract member Name: string
    abstract member Match: tokens: Tokenizer array -> state: State -> AST
    abstract member Identity: string with get
and AST = 
    | Token  of Tokenizer
    | Nested of string * AST seq

type Spec'LiteralParser = 
    | R of Regex
    | N of Name     : string  // Name is also const string
    | V of StrValue : string
    | C of ConstStr : string

type LiteralParser(it: Spec'LiteralParser) = 
    member this.it = it
    member private this.s_name = 
        match it with 
        | R regex -> sprintf "R'%s'" (regex.ToString()) 
        | N name  -> name
        | C str
        | V str   -> sprintf "C'%s'" str
        |> Const'Cast
    interface Parser with
        member this.Identity = raise (System.NotImplementedException())
        member this.Match tokens state = raise (System.NotImplementedException())
        member this.Name = raise (System.NotImplementedException())


type Spec'ComposedParser = 
    | Single      of LiteralParser 
    | Or          of ComposedParser list
    | And         of ComposedParser list
    | Seq         of ComposedParser * least: int * most: int
    | Named       of ComposedParser
    | NameBinding of Spec'LiteralParser

and  ComposedParser(it: Spec'ComposedParser) = 
    interface Parser with
        member this.Identity = raise (System.NotImplementedException())
        member this.Match tokens state = raise (System.NotImplementedException())
        member this.Name = raise (System.NotImplementedException())


     
     
        

    


              

        

   
   

