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

and parser = 
    abstract member Match: Tokenizer array -> State -> LanguageArea -> Result
    abstract member Name: string


and Parser =  
    static member name (parser': parser) = parser'.Name
    static member match' (tokens: Tokenizer array) (state: State) (lang : LanguageArea) (parser': parser) =
        parser'.Match tokens state lang
    

        
    static member optimize'branches (ands: And list): And list =
        let rec inner (ands: And list): And list = 
            
            ands 
            |> List.groupBy (fun it -> it.structure |> List.head |> Parser.name)
            |> fun it ->
              
              if it |> Seq.map (snd >> List.length) |> Seq.forall ((=) 1) 
              then ands
              else 
              let rec process' (ret: And list) (from: (string * And list) list)=
                match from with 
                | []               -> ret |> List.rev
                | (_, atoms) :: xs -> 
                    if atoms |> List.length |> (=) 1 
                    then 
                        atoms.[0]
                    else 
                        let strucures     = atoms     |> List.map (fun it -> it.structure)
                        let head          = strucures |> List.head |> List.head;
                        let tails         = strucures |> List.map List.tail
                        let opt, others   = tails     |> List.partition (List.length >> ((=) 0))

                        match others |> List.length with 
                        | 0 ->
                            And [head]
                        | _ ->
                            let sndloc = 
                                        others 
                                        |> List.map And 
                                        |> inner
                                        |> Or 
                                        |> ``Branch Atom`` |> Atom
                                
                
                            if List.length opt |> ((=) 0)
                            then 
                                And [head; sndloc]
                            else 
                                And [head; ``SubSequence Atom``(sndloc, 0, 1) |> Atom]
                    |>
                    function 
                    | res ->
                        process' (res :: ret) xs
                     
              process' [] it
        inner ands
                        
    static member optimise (or': Or): Or =
        
        let optimized = or'.structure |> Parser.optimize'branches
        if optimized &= or'.structure 
        then 
            or'
        else 
            Or(optimized)




and Literal(literal: ``Literal Spec``) =
    class 
        let name =
            match literal with 
            | RegExp  regex                   -> "R",  regex.ToString()
            | ``Not RegExp`` regex            -> "~R", regex.ToString()
            | Name  name                      -> "N",  name
            | ``Not Name`` name               -> "~N", name
            | ValueStr  str                   -> "L",  str
            | ``Not ValueStr`` str            -> "~L", str
            | ConstStr  str                   -> "C",  str 
            | ``Not ConstStr`` str            -> "~C", str
            | ``Name and Value``(name, value) -> sprintf "<%s>" name, value
            | ``Func Predicate`` f            -> "", f.ToString() |> sprintf "@%s"
            |> function
                | ("",  fnName) -> fnName |> Const'Cast
                | (prefix, str) -> 
                    sprintf "%s'%s'" prefix (str |> Const'Cast)


        let core = literal
        
        member this.structure = core
    end 

    interface parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            let idx = state.trace.Count - 1
            if tokens.Length <= idx then Unmatched
            else 
            let token = tokens.[idx]
            match this.structure with
            | RegExp regex               ->
                (regex.Match token.value).Success
            | ``Not RegExp`` regex              ->
                not (regex.Match token.value).Success
            | Name name                ->
                name &= token.name 
            | ``Not Name`` name               ->
                name &!= token.name
            | ValueStr runtime'str         ->
                runtime'str = token.value
            | ``Not ValueStr`` runtime'str        ->
                runtime'str <> token.value
            | ConstStr const'str           ->
                const'str &= token.value
            | ``Not ConstStr`` const'str          ->
                const'str &!= token.value
            | ``Name and Value``(name, const'str)   ->
                name &= token.name && const'str &= token.value
            | ``Func Predicate`` predicate          ->
                predicate(token)
            |>
            function 
             | true -> 
                state.trace.NewOne()
                token |> AST.Single |> Matched 
             | _    ->
                Unmatched

           
            

and ``Atom Spec`` =
    | ``Literal Atom``         of Literal
    | ``SubSequence Atom``     of Atom * int * int
    | ``Branch Atom``          of Or
    | ``Named Atom``           of Named
    | ``Name Binding Atom``    of string * Atom

and Atom(union: ``Atom Spec``) =
    class 
        // binding local var
        let (core, name) =
            match union with 
            | ``Literal Atom`` lit                   -> 
                lit 
                |> Parser.name
                |> fun it -> union, it
            | ``Named Atom``   named             -> 
                named 
                |> Parser.name
                |> fun it -> union, it

            | ``Branch Atom`` branch ->
                // refactor for optimise
                let branch' = branch|> Parser.optimise 
                branch' |> ``Branch Atom`` , branch' |> Parser.name |> sprintf "(%s)"

            | ``SubSequence Atom`` (atom, least, most) ->
                 let name = atom |> Parser.name
                 match least, most with 
                 | 1, 1 -> 
                    atom.structure, name 
                 | 0, 1 ->
                    union, sprintf "%s?" (name)
                
                 | 0, -1 ->
                    union, sprintf "%s*" (name)
                  
                 | 1, -1 ->
                    union, sprintf "%s+" (name)
                 | _, -1 ->
                    union, sprintf "%s{%d}" name least 
                 | _     -> union, sprintf "%s{%d %d}" name least most
                 
                 |> tuple.apply (id, Const'Cast)

                 
            | ``Name Binding Atom``(as'name, atom) ->
                atom |> Parser.name |> fun it -> sprintf "%s as %s" it as'name 
                |> Const'Cast
                |> fun it -> union, it

        member this.structure = core
    end

    interface parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            match this.structure with 
            | ``Literal Atom`` lit 
                ->
                lit |> Parser.match' tokens state lang
            
            | ``Named Atom`` named 
                ->
                named |> Parser.match' tokens state lang 
            
            | ``Branch Atom`` branch ->
                branch |> Parser.match' tokens state lang

            | ``Name Binding Atom``(name, atom) 
                ->
                atom |> Parser.match' tokens state lang
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

            | ``SubSequence Atom``(atom, least, most) 
                ->
                let history = state.Commit()

                let rec repeat'(parsed: AST CList) (i: int) = 
                    if i = most then Matched (Nested parsed)
                    else 
                    atom
                    |> Parser.match' tokens state lang 
                    |>

                    function 
                     | Unmatched 
                        -> 
                        state.Reset history
                        if i < least then Unmatched
                        else Matched (Nested parsed)

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
            atoms <*> (id, Seq.map Parser.name>> String.concat " " >> Const'Cast)
        member this.structure = atoms
    end 
    interface parser with 
        member this.Name: string = name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
           let history = state.Commit()
           let rec for'each (parsed: AST CList) : (Atom list) -> Result = 
                function
                | []    -> parsed |> Nested |> Matched
                | x::xs ->
                    x
                    |> Parser.match' tokens state lang 
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
            |> Parser.optimize'branches
            |> fun core -> core <*> (id, Seq.map Parser.name >> String.concat " | " >> Const'Cast)
        member this.structure = core
    end

    interface parser with 
        member this.Name: string =  name
        member this.Match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
            let rec m_match (tokens: Tokenizer array) (state: State) (lang : LanguageArea) = 
                let history = state.Commit()

                let rec for'each: (And list) -> Result = 
                    function
                    | []    -> Unmatched
                    | x::xs ->
                        x 
                        |> Parser.match' tokens state lang 
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

    interface parser with 
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

                let ``or``  = lang.[this |> Parser.name]
                let history = state.Commit()
                let context = state.context
                let is'custom = state.useCustomStructure

                state.useCustomStructure <- restructured.IsSome
                
                ``or`` 
                |> Parser.match' tokens state lang
                |> 
                function 
                 | Unmatched               
                    -> Unmatched

                 | Matched(Nested astList) 
                    ->
                    AST.Node(this |> Parser.name, astList)

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
                            this |> Parser.name 
                            <..> astList
                            |> AST.Node
                            |> Matched
                        | _     -> failwith "Impossible"

                    if state.lrParser.Value &!= this 
                    then 
                        FindLR stacked'
                    else 

                    let final' = 
                        match ``or`` |> Parser.match' tokens state lang with 
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
                
               
           
                
            
                
                
    