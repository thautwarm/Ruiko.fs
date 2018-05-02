module Parser
open Ruikowa.CSharp
open Tokenizer
open Utils
open AST
open Result
open System




type  State  = {
    trace: string Trace
    mutable context: Map<string, AST>
}
    with member this.Commit() = this.trace.Commit(), this.context
         member this.Reset(trace'record, context'record): unit =
            this.trace.Reset trace'record
            this.context <- context'record

type LanguageArea = CDict<string, Or>

and result = Result<Parser, AST>

and Parser = 
    abstract member Match: Tokenizer array -> State -> LanguageArea -> result
    abstract member Name: string

and Parser'' =  
    static member ToName (parser: Parser) = parser.Name
    static member Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) (parser: Parser) =
        parser.Match tokens state lang
    
    static member Optimized (ands: And array): And array = raise NotImplemented



and Literal = 
    
    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  raise NotImplemented

and Atom'Core =
    | Lit     of Literal
    | Seq     of Or * int * int
    | Named   of Named
    | Binding of string * Atom

and Atom(union: Atom'Core) =
    class 
        // binding local var
        let (core, name) =
            match union with 
            | Lit lit                   -> 
                lit |> Parser''.ToName |> Const'Cast
                |> fun it -> union, it
            | Named   named             -> 
                named |> Parser''.ToName |> Const'Cast
                |> fun it -> union, it
            | Seq (``or``, least, most) ->
                ``or`` 
                 |> Parser''.ToName
                 |> fun it -> 
                    let core = Seq(Or(``or``.core |> Parser''.Optimized), least, most)
                    match least, most with
                    | 1,  1  -> sprintf "(%s)"        it
                    | 0,  1  -> sprintf "[%s]"        it
                    | 0, -1  -> sprintf "(%s)*"       it
                    | 1, -1  -> sprintf "(%s)+"       it
                    | _, -1  -> sprintf "(%s){%d}"    it least 
                    | _      -> sprintf "(%s){%d %d}" it least most
                    |> fun it -> (core, it)
            | Binding(as'name, atom) ->
                atom |> Parser''.ToName |> fun it -> sprintf "%s as %s" it as'name 
                |> fun it -> union, it
            
    end
    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        

and And(atoms: Atom array) = 
    class 
        member this.name      = atoms |> Seq.map Parser''.ToName |> String.concat " " |> Const'Cast
        member this.structure = atoms
    end 
    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  raise NotImplemented
        
and Or(ands: And array) = 
    class
        let (core, name) = 
            ands 
            |> Parser''.Optimized
            |> fun core -> core <*> (id, Seq.map Parser''.ToName >> String.concat " | " >> Const'Cast)
    end

    interface Parser with 
        member this.Name: string =  raise NotImplemented
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())

and Named(name: string) = 
    class 
        member this.name = name
    end 
    interface Parser with 
        member this.Name: string =  this.name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException()) 
    