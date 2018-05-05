module Parser
open Ruikowa.CSharp
open Tokenizer
open Utils
open AST
open Result
open System
open System.Text.RegularExpressions
open System.Collections



type State  = {
    trace: Named Trace
    mutable context  : Map<string, AST>
    mutable lrParser : Named option
    mutable useCustomStructure : bool
}
    with member this.Commit() = this.trace.Commit(), this.context
         member this.Reset(trace'record, context'record): unit =
            this.trace.Reset trace'record
         member this.Contains(named: Named) = 
            this.trace.FindSameObj named |> (<>) -1
         static member New () = 
            let new' = {trace = Trace(); context = Map[] ; lrParser = None; useCustomStructure=false;}
            new'.trace.NewOne()
            new'
    
and When = CDict<string, (State -> bool)>
and With = CDict<string, (State -> AST -> bool)>
and LanguageArea = CDict<string, Or>

and Result = 
    | Unmatched
    | Matched of AST
    | FindLR  of (AST -> Result)

and Parser = 
    abstract member Match: Tokenizer array -> State -> LanguageArea -> Result
    abstract member Name: string

and Parser'' =  
    static member ToName (parser: Parser) = parser.Name
    static member Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) (parser: Parser) =
        parser.Match tokens state lang
    
    static member Optimized (ands: And list): And list = ands //TODO



and Literal(literal: Literal'Core) =
    class 
        let name =
            match literal with 
            | R  regex        -> "R",  regex.ToString()
            | R' regex        -> "~R", regex.ToString()
            | N  name         -> "N",  name
            | N' name         -> "~N", name
            | L  str          -> "L",  str
            | L' str          -> "~L", str
            | C  str          -> "C",  str 
            | C' str          -> "~C", str
            | NC(name, value) -> sprintf "<%s>" name, value
            | Fn f            -> "", f.ToString() |> sprintf "@%s"
            |> function
                | ("",  fnName) -> fnName |> Const'Cast
                | (prefix, str) -> 
                    sprintf "%s'%s'" prefix (str |> Const'Cast)


        let core = literal
        
        member this.structure = core
    end 

    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            let idx = state.trace.Count - 1
            if tokens.Length <= idx then Unmatched
            else 
            let token = tokens.[idx]
            match this.structure with
            | R regex               ->
                (regex.Match token.value).Success
            | R' regex              ->
                not (regex.Match token.value).Success
            | N name                ->
                name &= token.name 
            | N' name               ->
                name &!= token.name
            | L runtime'str         ->
                runtime'str = token.value
            | L' runtime'str        ->
                runtime'str <> token.value
            | C const'str           ->
                const'str &= token.value
            | C' const'str          ->
                const'str &!= token.value
            | NC(name, const'str)   ->
                name &= token.name && const'str &= token.value
            | Fn predicate          ->
                predicate(token)
            |>
            function 
             | true -> 
                state.trace.NewOne()
                token |> AST.Single |> Matched 
             | _    ->
                Unmatched

           
            

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
                lit 
                |> Parser''.ToName
                |> fun it -> union, it
            | Named   named             -> 
                named 
                |> Parser''.ToName
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
                    |> Const'Cast
                    |> fun it -> (core, it)
            | Binding(as'name, atom) ->
                atom |> Parser''.ToName |> fun it -> sprintf "%s as %s" it as'name 
                |> Const'Cast
                |> fun it -> union, it

        member this.structure = core
    end

    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            match this.structure with 
            | Lit lit 
                ->
                lit |> Parser''.Match tokens state lang
            
            | Named named 
                ->
                named |> Parser''.Match tokens state lang 

            | Binding(name, atom) 
                ->
                atom |> Parser''.Match tokens state lang
                |> function
                | Unmatched      
                    -> Unmatched
                | Matched ast    
                    -> state.context  <- state.context.Add (name,  ast)
                       Matched ast
                | FindLR stacked 
                    ->
                    fun (ast: AST) ->
                        state.context <- state.context.Add (name,  ast)
                        Matched ast
                    |> FindLR

            | Seq(``or``, least, most) 
                ->
                let history = state.Commit()

                let rec repeat'(parsed: AST CList) (i: int) = 
                    if i = most then if i < least then Unmatched else Matched (Nested parsed)
                    else 
                    ``or``
                    |> Parser''.Match tokens state lang 
                    |>
                    function 
                     | Unmatched 
                        -> 
                        state.Reset history
                        Unmatched
                     | Matched(Nested astList) 
                        ->
                        parsed.AddRange astList
                        repeat' parsed (i+1)
                     | Matched ast
                        -> 
                        parsed.Add ast
                        repeat' parsed (i+1)
                     | FindLR stacked 
                        ->
                        fun (ast: AST) -> 
                            let parsed' = CList(parsed)
                            match ast with 
                            | Nested astList ->
                                parsed'.AddRange astList
                            | _              ->
                                parsed'.Add ast
                            repeat' parsed' (i+1)
                        |> FindLR

                repeat' (CList(least)) 0
                    

                
                    
and And(atoms: Atom list) = 
    class 
        let (core, name) =
            atoms <*> (id, Seq.map Parser''.ToName>> String.concat " " >> Const'Cast)
        member this.structure = atoms
    end 
    interface Parser with 
        member this.Name: string = name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
           let history = state.Commit()
           let rec for'each (parsed: AST CList) : (Atom list) -> Result = 
                function
                | []    -> parsed |> Nested |> Matched
                | x::xs ->
                    x
                    |> Parser''.Match tokens state lang 
                    |> 
                    function 
                    | Unmatched   -> 
                        state.Reset history
                        Unmatched
                    | Matched (Nested astList) ->
                        parsed.AddRange astList
                        for'each parsed xs 
                    | Matched (ast) ->
                        parsed.Add ast 
                        for'each parsed xs
                    | FindLR stacked ->
                        fun (ast: AST) ->
                            let parsed' = CList (parsed) // copy
                            match stacked ast with
                            | Unmatched -> Unmatched
                            | Matched (Nested astList) ->
                                parsed'.AddRange astList 
                                for'each parsed' xs
                            | Matched ast ->
                                parsed'.Add ast
                                for'each parsed' xs 
                            | _ -> failwith "Impossible"
                        |> FindLR
                         
                          

           for'each (CList(this.structure.Length)) this.structure
        
and Or(ands: And list) = 
    class
        let (core, name) = 
            ands 
            |> Parser''.Optimized
            |> fun core -> core <*> (id, Seq.map Parser''.ToName >> String.concat " | " >> Const'Cast)
        member this.structure = core
    end

    interface Parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            let rec m_match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
                let history = state.Commit()

                let rec for'each: (And list) -> Result = 
                    function
                    | []    -> Unmatched
                    | x::xs ->
                        x 
                        |> Parser''.Match tokens state lang 
                        |> 
                        function 
                        | Unmatched -> 
                            state.Reset history
                            for'each(xs)
                        | _   as sub -> sub

                for'each(this.structure)
            m_match tokens state lang

                
            
            

and Named(name: string, 
          (**whether to enter this parser.**)
          when': When,
          (**To judge if current parser succeeds in parsing after context-free processsing.**)
          with': With,

          lang: LanguageArea,

          restructured: (State -> AST) option) = 
    
    let s_name = name |> Const'Cast

    interface Parser with 
        member this.Name: string =  s_name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            let rec s_match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 

                if (state.lrParser.IsSome && state.lrParser.Value &= this) 
                    || when'.Values |> Seq.exists (fun predicate -> not <| predicate state)
                then Unmatched
                else

                if state.Contains this 
                then 
                    state.lrParser <- Some(this)
                    FindLR (fun it -> Matched it)
                else
                state.trace.Add this

                let ``or``  = lang.[this |> Parser''.ToName]
                let history = state.Commit()
                let context = state.context
                let is'custom = state.useCustomStructure

                state.useCustomStructure <- restructured.IsSome
                
                ``or`` 
                |> Parser''.Match tokens state lang
                |> 
                function 
                 | Unmatched               
                    -> Unmatched

                 | Matched(Nested astList) 
                    ->
                    AST.Named(this |> Parser''.ToName, astList)

                    |> fun result ->
                    if with'.Values |> Seq.exists (fun predicate -> not <| predicate state result)
                    then 
                        Unmatched
                    else 
                        result |> Matched
                
                 
                 
                 | FindLR stacked 
                    ->
                    let stacked' (ast: AST) =
                        match stacked ast with 
                        | Unmatched -> Unmatched
                        | Matched(Nested astList) ->
                            this |> Parser''.ToName 
                            <..> astList
                            |> AST.Named
                            |> Matched
                        | _     -> failwith "Impossible"

                    if state.lrParser.Value &!= this 
                    then 
                        FindLR stacked'
                    else 

                    let final' = 
                        match ``or`` |> Parser''.Match tokens state lang with 
                        | Unmatched    -> Unmatched
                        | Matched head ->

                            let rec stack'jumping(head_in: AST) = 
                                match stacked' head_in with
                                | Unmatched        -> head_in
                                | Matched head_out ->
                                    stack'jumping head_out
                                | _                -> failwith "Impossible"

                            head |> stack'jumping |> Matched

                        | _            -> 
                            failwith "Impossible"
                    
                    state.lrParser <- None
                    final'

                 | _ 
                    -> failwith "Impossible"
                
                 
                 |> fun return' -> 
                     match return' with 
                     | Unmatched -> 
                        state.Reset history 
                        Unmatched
                     | _         -> 
                        if state.useCustomStructure then
                            restructured.Value(state) |> Matched 
                        else 
                            return'
                 |> fun return' ->
                     state.context <- context
                     state.useCustomStructure <- is'custom
                     return'
                    
                    
                    
                     
                 
            
            s_match tokens state lang
                
               
           
                
            
                
                
    