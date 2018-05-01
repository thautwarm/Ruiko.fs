module Parser
open Ruikowa.CSharp
open System.Text.RegularExpressions
open System

let inline (&=) a b = obj.ReferenceEquals(a, b)

type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let ConstStrPool: CDict<string, string> = new CDict<string, string>()
let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (true, _) -> 
        ConstStrPool.Add(s, s)
        s
    | (false, r) -> r

let range = System.Linq.Enumerable.Range


type Tokenizer = {
    name   : string
    value  : string
    colno  : int
    lineno : int
}

type ArrayView<'T> = {
    arr   : 'T array
    start :  int
}

let inline (|StartWith|_|)(view: Tokenizer ArrayView) = 
    if view.start >= view.arr.Length then None
    else Some(view.arr.[view.start])

    
type LanguageArea = CDict<string, Parser>

and Status = Matched | Unmatched | Finsihed

and  Result(status: Status, ast:AST) = 
    member this.status = status 
    member this.ast    = ast
    static member finished            = Result(Finsihed , Nil)
    static member unmatched           = Result(Unmatched, Nil)
    static member matched (ast: AST) = Result(Matched, ast)
and  State  = {
    mutable count: int
    mutable trace: Parser Trace
    mutable context: Map<string, AST>
}
and Parser  = 
    abstract member Lang: LanguageArea

    abstract member Name: string
   
    abstract member Match: tokens: Tokenizer array -> state: State -> Result

and AST     = 
    | Token  of Tokenizer
    | Nested of string * AST CList
    | Zipped of AST CList
    | Nil


let inline Match (tokens: Tokenizer array) (state: State) (parser: Parser) = parser.Match tokens state


let inline GetParserName (a: Parser) = a.Name
    

type Spec'LiteralParser = 
    | R of Regex
    | N of Name     : string  // Name is also const string
    | V of StrValue : string
    | C of ConstStr : string

type LiteralParser(it: Spec'LiteralParser, lang: LanguageArea) = 
    member this.it = it
    

    interface Parser with
        member this.Lang = lang

        member this.Name = 
            match it with 
            | R regex -> sprintf "R'%s'" (regex.ToString()) 
            | N name  -> name
            | C str
            | V str   -> sprintf "C'%s'" str
            |> Const'Cast

        member this.Match tokens state = 
            match {arr=tokens; start=state.count} with 
            | StartWith head -> 

                if match it with 
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

 
let Transaction (``what's true``: Result -> bool) (``true action``) (``false action``)  (result: Result) = 
    if ``what's true`` result then
        ``true action``(result)
        true
    else 
        ``false action``(result)
        false


type Spec'ComposedParser = 
    | Single      of LiteralParser
    | Or          of ComposedParser array
    | And         of ComposedParser array
    | Seq         of ComposedParser * least: int * most: int
    | Named       of name: string
    | NameBinding of name: string * ComposedParser



and ComposedParser(it: Spec'ComposedParser, lang: LanguageArea) =
    interface Parser with
        member this.Lang = lang
        
        member this.Name = 
            match it with 
            
            | Single case ->
                GetParserName case
            | Or cases   ->
                cases
                |> Seq.map GetParserName
                |> String.concat "|"
            | And cases  ->
                cases 
                |> Seq.map GetParserName
                |> String.concat " "
            | Seq (case, least, most) ->
                let name = case |> GetParserName |> sprintf "(%s)"
                match (least, most) with
                | 1,  1 ->                       name
                | 0,  1 ->  sprintf "[%s]"       name
                | _, -1 ->  sprintf "%s{%d}"     name least
                | _     ->  sprintf "%s{%d, %d}" name least most
            | Named name -> 
                name
            | NameBinding(name, case) ->
                sprintf "%s as %s" (GetParserName case) name
        
        member this.Match tokens state = 
            if tokens.Length <= state.count
            then Result.finished
            else match it with 
                 | Single case ->
                    case |> Match tokens state 

                 | Or cases ->
                    let history = state.trace.Commit();
                    let transaction = 
                            Transaction 
                                (fun each -> each.status = Matched)
                                (fun _    -> ())
                                (fun _    -> state.trace.Reset history)

                                
                    let some =
                        cases 
                        |> Seq.map (Match tokens state)
                        |> Seq.tryFind transaction
                    
                    match some with 
                    | None       -> Result.unmatched
                    | Some value -> value

                 | And cases ->

                    let parsed      = CList<AST>(cases.Length)
                    let history     = state.trace.Commit()
                    let transaction = 
                        Transaction 
                            (fun each -> each.status <> Matched)
                            (fun _    -> state.trace.Reset history)
                            (fun each -> parsed.Add each.ast)
                    
                    
                    let result = 
                        cases
                        |> Seq.map (Match tokens state)

                    let some = 
                        result |> Seq.tryFind transaction
                    
                    match some with
                    | None -> 
                        Nested ((this :> Parser).Name, parsed)
                        |> Result.matched

                    | Some value ->
                        value

                 | Seq (case, least, most) ->
                    let mutable count = 0
                    let action = fun _ -> 
                            case |> Match tokens state
                       
                    if least = 1
                    
                    then raise (NotImplementedException())
                    else 
                    let parsed      = CList<AST>(least)
                    
                    let result = range(0, least)
                                 |> Seq.map action
                    
                    let history =  state.trace.Commit()
                    let transaction = 
                        Transaction 
                            (fun each -> each.status <> Matched)
                            (fun _    -> state.trace.Reset history)
                            (fun each -> parsed.Add each.ast)

                    result 
                    |> Seq.tryFind transaction
                    |> fun some -> 
                        match some with
                        | None -> 
                            result
                            
                                    


                    
                    
                        
                        
                        
                    
                    
                    

                    
                

                   
                   

                     
                    

                    
                
                
           



     
     
        

    


              

        

   
   

