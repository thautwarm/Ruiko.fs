module Parser3

open Ruikowa.CSharp
open RuikoCollections
open ConstStringPool
open Utils
open System.Text.RegularExpressions
open System

let range = System.Linq.Enumerable.Range

type Tokenizer = {
    name   : string
    value  : string
    colno  : int
    lineno : int
}

let inline (|StartWith|_|)(view: Tokenizer ArrayView) = 
    if view.start >= view.arr.Length then None
    else Some(view.arr.[view.start])

type Parser' = 
    static member ToName (parser: IParser) = parser.Name
    static member Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) (parser: IParser) =
        parser.Match tokens state lang
        

and  AST  = 
    | Token    of Tokenizer
    | Nested   of AST CList
    | Named    of string * AST
    | Zipped   of AST CList
    | Nil

and Status = Matched | Unmatched | Finsihed | LeftRecursive of string * string

and LanguageArea = CDict<string, Or>

and  Result(status: Status, ast:AST) = 
    member this.status = status 
    member this.ast    = ast
    static member finished               = Result(Finsihed , AST.Nil)
    static member unmatched              = Result(Unmatched, AST.Nil)
    static member matched (ast: AST)     = Result(Matched,   ast)
    static member findLR  (name: string) = Result(LeftRecursive(name, ""), Nil)

and  State  = {
    trace: string Trace
    mutable context: Map<string, AST>
}
    with member this.Commit() = this.trace.Commit(), this.context
         member this.Reset(trace'record, context'record): unit =
            this.trace.Reset trace'record
            this.context <- context'record

        

and IParser =
    abstract member Match: Tokenizer array -> State -> LanguageArea -> Result
    abstract member Name: string
    
and Literal'Core = 
    | R of Regex 
    | N of string // Name
    | V of string // runtime string
    | C of string // const string

and Named(name      : string,
          When      : State -> bool, 
          Constraint: AST   -> State -> bool,
          Refactor  : State -> AST) =
    member this.core = name |> Const'Cast
    interface IParser with 
        member this.Name: string =  this.core
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            if not (When state) 
            then Result.unmatched
            else
            
            let last = state.trace.Find this.core
            if last <> -1 
            then 
                Result.findLR(this |> Parser'.ToName)
            else 

            let history = state.Commit()
            let ``or``  = lang.[this.core]
            let result  = ``or`` |> Parser'.Match tokens state lang
            if result  &= Result.unmatched || result &= Result.finished
            then 
                state.Reset history
                result
            else 
            match result.status with
            | LeftRecursive (name, branch) -> 
                if name &= (this |> Parser'.ToName) 
                then 
                    state.Reset history

                    let mutable lrHead    : AST    = Nil
                    let mutable lrHeadRes : Result = Result.unmatched

                    let diedBranch = ``or``.core |> Array.find (fun case ->  branch &= (case |> Parser'.ToName))

                    let available'some = 
                        ``or``.core 
                        |> Array.tryFind (
                           fun each -> 
                             if branch &!= (each |> Parser'.ToName)
                             then 
                                lrHeadRes <- each |> Parser'.Match tokens state lang
                                lrHead    <- lrHeadRes.ast
                                if lrHeadRes &= Result.finished
                                then 
                                    state.Reset history
                                    true 
                                elif lrHeadRes.status = Matched
                                then true
                                else false 
                             else 
                                state.Reset(history)
                                false)
                    if lrHeadRes.status = Matched 
                    then 
                        match available'some with
                        | None ->
                            sprintf "Cannot resolve LR cases at %s. Please define a new case of " (this |> Parser'.ToName)
                            |> failwith
                        | Some available'branch ->
                            let arrLen = (Array.length diedBranch.core);

                            let rec handleLR () = 
                                   let lrZipped = CList([lrHead])
                                   range(1, arrLen)
                                   |> Seq.map (fun idx ->
                                        let atom    = diedBranch.core.[idx]
                                        let history = state.Commit()
                                        let result  = atom |> Parser'.Match tokens state lang
                                        if result &= Result.finished then 
                                            state.Reset history
                                            Finsihed
                                        elif result.status = Matched 
                                        then
                                            match result.ast with 
                                            | Zipped astList -> lrZipped.AddRange astList
                                            | _              -> lrZipped.Add result.ast
                                            Matched
                                        else 
                                            result.status)
                                   |> Seq.tryFindIndex (fun it -> it = Unmatched || it = Finsihed)
                                   |> fun it ->
                                        if it.IsNone  
                                        then
                                            lrHead <- AST.Named(this |> Parser'.ToName, lrZipped |> Nested)
                                            handleLR()
                                        else lrHead 
                            
                            handleLR() |> Result.matched

                               

                             
                            

                    else 
                        state.Reset history
                        lrHeadRes

                    
                else 
                    result
            | _                  -> 
                AST.Named(this |> Parser'.ToName, result.ast)
                |> Result.matched


                
                





and Literal(literal: Literal'Core) =
   
    let s_name = 
        match literal with 
        | R regex ->
            regex.ToString() |> sprintf "R'%s'"
        | N name  -> 
            name
        | V str   -> 
            str |> sprintf "V'%s'"
        | C str   ->
            str |> sprintf "C'%s'"
        |> Const'Cast
        

    member this.core = Literal
    
    interface IParser with 
        member this.Name: string =  s_name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())        
   
and Atom'Core = 
    | Literal of Literal
    | Named   of Named
    | Seq     of Or * int * int
    | Binding of string * Atom 

and Atom(atom: Atom'Core) =
    let s_name = 
        match atom with 
        | Literal lit    -> lit   |> Parser'.ToName
        | Named   named  -> named |> Parser'.ToName
        | Seq(``or``, least, most) ->
            ``or`` :> IParser 
             |> fun it -> it.Name 
             |> fun it -> 
                match least, most with
                | 1,  1  -> sprintf "(%s)"        it
                | 0,  1  -> sprintf "[%s]"        it
                | 0, -1  -> sprintf "(%s)*"       it
                | 1, -1  -> sprintf "(%s)+"       it
                | _, -1  -> sprintf "(%s){%d}"    it least 
                | _      -> sprintf "(%s){%d %d}" it least most
        | Binding(as'name, atom) ->
            atom
            |> Parser'.ToName
            |> fun it -> sprintf "%s as %s" it as'name 
        |> Const'Cast

    member this.core = atom

    interface IParser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  s_name

and And(atoms : Atom array) = 
    
    let s_name = 
        atoms
        |> Seq.map Parser'.ToName
        |> String.concat " "
        |> Const'Cast

    member this.core: Atom array = atoms

    interface IParser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  s_name

and Or(ands: And array) = 
    let s_name = 
        ands
        |> Seq.map Parser'.ToName
        |> String.concat "|"
        |> Const'Cast

    member this.core: And array = ands

    interface IParser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  s_name

    
