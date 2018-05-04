module Parser
open Ruikowa.CSharp
open Tokenizer
open Utils
open AST
open Result
open System
open System.Text.RegularExpressions


let (|Finded|_|) (i: int) = 
    if i <> -1
    then Some(i)
    else None


type  State  = {
    trace: Named Trace
    mutable context: Map<string, AST>
}
    with member this.Commit() = this.trace.Commit(), this.context
         member this.Reset(trace'record, context'record): unit =
            this.trace.Reset trace'record
            this.context <- context'record
         member this.Find(named: Named) = 
            this.trace.FindSameObj named 


and LanguageArea = CDict<string, Or>

and result = Result<Named, AST>

and Parser = 
    abstract member Match: Tokenizer array -> State -> LanguageArea -> result
    abstract member Name: string

and Parser'' =  
    static member ToName (parser: Parser) = parser.Name
    static member Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) (parser: Parser) =
        parser.Match tokens state lang
    
    static member Optimized (ands: And array): And array = ands

and Literal'Core = 
    | R      of Regex
    | R'     of Regex
    
    | N      of string // Name
    | N'     of string 
    
    | L      of string // Runtime String 
    | L'     of string

    | C      of string // Const String
    | C'     of string 

and Literal(literal: Literal'Core) =
    class 
        let name =
            match literal with 
            | R  regex -> "R",  regex.ToString()
            | R' regex -> "~R", regex.ToString()
            | N  name  -> "N",  name
            | N' name  -> "~N", name
            | L  str   -> "L",  str
            | L' str   -> "~L", str
            | C  str   -> "C",  str 
            | C' str   -> "~C", str
            |> fun (prefix, str) -> sprintf "%s'%s'" prefix (str |> Const'Cast)
        let core = literal
        
        member this.structure = core
    end 

    interface Parser with 
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        member this.Name: string =  name

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
                    let core = Seq(Or(``or``.structure |> Parser''.Optimized), least, most)
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

        member this.structure = core
    end
    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        

and And(atoms: Atom array) = 
    class 
        let (core, name) =
            atoms <*> (id, Seq.map Parser''.ToName>> String.concat " " >> Const'Cast)
        member this.structure = atoms
    end 
    interface Parser with 
        member this.Name: string = name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())
        
        
and Or(ands: And array) = 
    class
        let (core, name) = 
            ands 
            |> Parser''.Optimized
            |> fun core -> core <*> (id, Seq.map Parser''.ToName >> String.concat " | " >> Const'Cast)
        member this.structure = core
    end

    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = raise (NotImplementedException())

and Named(name: string, lang: LanguageArea, 
          (**whether to enter this parser.**)
          when': (State -> bool) option,
          (**To judge if current parser succeeds in parsing after context-free processsing.**)
          with': (State -> AST -> bool) option) = 
    
    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
        
            if when' <..> false =??=> fun it -> it(state)
            then result.unmatched
            else

            match state.Find this with 
            | Finded begin' -> 
                state.trace
                    .GetSlice(begin', -1)
                    |> result.findLR
                
            | _  ->
                let ``or`` = lang.[this |> Parser''.ToName]

                

                result.unmatched
            
                
                
    