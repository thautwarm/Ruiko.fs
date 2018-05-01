module Parser2


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
    | Nested of string * AST CList
    | Nil

type IParser = 
    abstract member Match: tokens: Tokenizer array -> state: State -> lang: LanguageArea -> Result
    abstract member Name : string 

and Status = Matched | Unmatched | Finsihed | LeftRecursive of Parser<Named>

and LanguageArea = CDict<string, Parser<Top>>

and  Result(status: Status, ast:AST) = 
    member this.status = status 
    member this.ast    = ast
    static member finished            = Result(Finsihed , AST.Nil)
    static member unmatched           = Result(Unmatched, AST.Nil)
    static member matched (ast: AST)  = Result(Matched,   ast)

and  State  = {
    mutable count: int
    mutable trace: Parser<Top> Trace
    mutable context: Map<string, AST>
}
    
and Single = 
    | R of Regex
    | N of Name     : string  // Name is also const string
    | V of StrValue : string
    | C of ConstStr : string
    with 
    member this.Name = 
        match this with
        | R regex -> sprintf "R'%s'" (regex.ToString()) 
        | N name  -> name
        | C str
        | V str   -> sprintf "C'%s'" str
        |> Const'Cast

    member this.Match (tokens: Tokenizer array) (state: State) =
        match {arr=tokens; start=state.count} with 
        | StartWith head -> 
            if match this with 
                | R regex ->
                    (regex.Match head.value).Success
                | N name ->
                    name &= head.name
                | C str  ->
                    str &= head.value
                | V str  ->
                    str = head.value
                 
            then
                state.count <- state.count + 1
                Result.matched (Token head) 
            else Result.unmatched
                    
        | _ -> Result.finished
   
and Parser<'a when 'a> =
    | S of 'a
    
    //with
    //member this.Name: string = 
    //    raise (NotImplementedException())
    //member this.Match (tokens: Tokenizer array) (state: State) : Result =
    //    raise (NotImplementedException())

 
and Or = 
    | S of Parser<And> array
    with 
    member this.Name: string = raise (NotImplementedException())
    member this.Match (tokens: Tokenizer array) (state: State) : Result = raise (NotImplementedException())

and Seq = 
    | S of Parser<Top> array * least: int * most: int
    with 
    member this.Name: string = raise (NotImplementedException())
    member this.Match (tokens: Tokenizer array) (state: State) : Result = raise (NotImplementedException())

and Named = 
    | S of string
    with 
    member this.Name: string = raise (NotImplementedException())
    member this.Match (tokens: Tokenizer array) (state: State) : Result = raise (NotImplementedException())

and Top = 
    | Seq   of Seq 
    | Named of Named
    | Single of Single
    with 
    member this.Name: string = raise (NotImplementedException())
    member this.Match (tokens: Tokenizer array) (state: State) : Result = raise (NotImplementedException())

and And = 
    | S of Parser<Top> array
    with 
    member this.Name =
       let (S arr) = this
       arr
       |> Seq.map (fun each -> each.Name)
       |> String.concat " "
    
    member this.Match (tokens: Tokenizer array) (state: State) =
        let (And.S arr) = this
        
        let parsed = CList<AST>(arr.Length);
        let history = state.trace.Commit()
         
        arr
        |> Seq.map (fun each -> (each.Match tokens state, each))
        |> Seq.tryFind(

            fun (result, parser) ->
                if result.status = Matched 
                then
                    match parser with 
                    | Parser.S(Seq(_)) ->
                        match result.ast with 
                        | Nested(_, astSeq) ->
                            parsed.AddRange astSeq
                        | _                 ->
                            failwith "Dependent type error."
                    | _     -> 
                        parsed.Add result.ast
                    false
                else
                    state.trace.Reset history
                    true
            )
        |> fun it ->
            if it.IsNone then parsed |> fun it -> Nested(this.Name, it) |> Result.matched
            else fst it.Value
       

       
            
        
        
             

      


            
    






