module ParserC
// The task is kind of heavy, 
// so use ParserC as a temporal alternative.
open Utils
open Ruikowa.CSharp
open Tokenizer
   
let JudgeByFnDict(fnDict: CDict<'T, ('G -> bool)>) (inp: 'G) = 
    fnDict.Values |> Seq.forall (fun it -> it inp)

type When = CDict<string, (Context -> bool)>
and With = CDict<string, (Context -> bool)>
and Refactor = (Context -> AST)
and Context = CDict<string, AST>
and LanguageArea = CDict<string, Parser>

and State  = {
    lang : LanguageArea
    trace: string Trace
    mutable context  : Context
    mutable generalContext: CDict<string, obj> // for more general usage, like evaling when parsing and using global context.
    mutable lrParser : string option
}
    with member this.Commit() = this.trace.Commit(), this.context
         member this.Reset(trace'record, context'record): unit =
            this.trace.Reset trace'record
         member this.Contains(named: string) = 
            this.trace.FindSameObj named |> (<>) -1
         static member New (lang: LanguageArea) = 
            let new' = {
                    lang = lang
                    trace = Trace() 
                    generalContext = CDict()
                    context = CDict()
                    lrParser = None 
                    }
            new'.trace.NewOne()
            new'
        
and Result = 
    | Unmatched
    | Matched of AST
    | FindLR  of (AST -> Context -> Result)
    

and Atom = 
    | Lit   of Literal
    | Bind  of string * Parser                 // -> ?. binding
    | Named of string * When * With * Refactor option // Node

and Composed = 
    | And   of Parser list          // -> Nested
    | Or    of Parser list          // -> ?
   

and Parser = 
    | Atom     of Atom
    | Composed of Composed
    | Seq      of Parser * int * int   // -> Nested 
    
    

and AST = 
    | Single of Tokenizer 
    | Node   of string * AST
    | Nested of AST CList
    | Nil 
    with 
    override this.ToString(): string = 
        let rec format (subject: AST) (idx: int) =
            match subject with
            | Single tk      -> 
                tk.ToString()
            | Node(name, ast)  ->
                let indent = String.replicate idx " "
                sprintf "%s[\n%s%s]" <| name  <| format ast (idx + name.Length) <| indent
            | Nested lst     -> 
                let indent = String.replicate idx " "
                lst 
                |> Seq.map (fun it -> format it (idx + 2)) 
                |> String.concat ("\n" + indent) 
                |> fun body -> sprintf "%s%s\n" indent body
            | Nil            -> 
                failwith "Try to access the Name of Empty AST"
        format this 0


let inline collectResult (ast: AST) (parsed: AST CList): unit =
    match ast with
    | Nested lst ->
        parsed.AddRange lst
    | _          ->
        parsed.Add ast

let mutable x = 0  
let debug(str: obj) = 
    printfn "at %d: %s" x (str.ToString())
    x <- x + 1


    
let rec Match (state: State) (tokens: Tokenizer array) (parser: Parser) = 
    

    match parser with 
    | Atom atom ->
        match atom with 
        | Lit lit ->
            let idx = state.trace.Count - 1

            if tokens.Length <= idx then Unmatched
            else 
            let token = tokens.[idx]
            match lit with 
            | R regex        -> regex.Match(token.value).Success 
            | R' regex       -> not <| regex.Match(token.value).Success 

                    
            | N name         -> name &= token.name
            | N' name        -> name &!= token.name 
            
            | C str          -> str &= token.value
            | C' str         -> str &!= token.value

            | L str          -> str = token.value
            | L' str         -> str <> token.value


            | NC (name, str) -> str &= token.value && name &= token.name 
            | Fn p           -> p(token) 
            |>  
             function 
             | true -> 
                state.trace.NewOne()
                Single token |> Matched
             | _    -> Unmatched

        | Bind(name, parser') ->
            match parser' |> Match state tokens with
            | Matched ast as it -> 
                state.context.Add(name, ast)
                it 
                
            | FindLR stackedFn ->
                let stackedFn' (ast : AST) (ctx: Context) = 
                    match stackedFn ast ctx with 
                    | Matched ast as it -> 
                        ctx.Add(name, ast)
                        it 
                    | _ -> Unmatched
                FindLR stackedFn'
            | _                 -> Unmatched

        | Named(name, when_, with_, refactor_) ->
            if state.Contains name 
            then 
                if state.lrParser.IsNone then
                    state.lrParser <- Some(name)
                    FindLR (
                        fun ast ctx -> 
                            if ctx |> JudgeByFnDict with_ |> not
                            then Unmatched
                            elif refactor_.IsNone then 
                               ast |> Matched
                            else 
                                refactor_.Value ctx |> Matched
                            )
                else
                    Unmatched
            else 
                state.trace.Add name
                let parser' = state.lang.[name]
                let context = state.context
                state.context <- CDict()
                if state.context |> JudgeByFnDict when_ |> not then
                    Unmatched
                else
                    parser' |> Match state tokens 
                |>
                function 
                | Matched ast         ->
                    if state.context |> JudgeByFnDict with_ |> not then
                        Unmatched
                    elif refactor_.IsNone then
                        Node(name, ast) |> Matched 
                    else 
                        refactor_.Value state.context |> Matched

                | FindLR (stackedFn)  ->
                    if state.lrParser.Value &!= name
                    then
                        let stackedFn' (ast: AST) (ctx: Context) = 
                            if ctx |> JudgeByFnDict when_ |> not then Unmatched
                            else
                            match stackedFn ast ctx with 
                            | Matched ast ->
                                if ctx |> JudgeByFnDict with_ |> not then
                                    Unmatched
                                elif refactor_.IsNone then
                                    Node(name, ast) |> Matched 
                                else 
                                    refactor_.Value ctx |> Matched
                        
                            | _           -> Unmatched
                        FindLR stackedFn'
                    else 
                        match parser' |> Match state tokens with
                        | Matched ast ->
                            let head = Node(name, ast)
                            
                            if state.context |> JudgeByFnDict with_ |> not then
                                Unmatched
                            else
                            let rec stack'jumping(head_in: AST) = 
                                if state.context |> JudgeByFnDict when_ |> not 
                                then head_in
                                else 
                                match stackedFn head_in state.context with
                                | Matched head_out ->
                                    if state.context |> JudgeByFnDict with_ |> not then
                                        head_in
                                    else 
                                        stack'jumping head_out
                                | _                -> head_in

                            
                            let res = Node(name, head |> stack'jumping) |> Matched
                            state.lrParser <- None
                            res
                        | _ -> Unmatched

                    
                | _                   -> Unmatched
                |> 
                function 
                | return' -> 
                    state.context <- context
                    return'
            
    
    | Composed composed ->
        let history = state.Commit()

        match composed with 
        | Or perspectives ->
            let rec foreach =
                function 
                | [] -> Unmatched
                | case::others ->
                    match case |> Match state tokens with 
                    | Matched ast as it ->
                        it 
                    | Unmatched ->
                        state.Reset history
                        foreach others
                    | _ as it -> it
            foreach perspectives

        | And composition ->
            let rec foreach (parsed: AST CList) = 
                function 
                | []          -> Matched <| Nested parsed
                | case::cases ->
                    match case |> Match state tokens with 
                    | Matched ast          ->
                        parsed |> collectResult ast
                        foreach parsed cases 
                    | FindLR stackedFn     ->
                        let stackedFn' (ast: AST) (ctx: Context) =
                            match stackedFn ast ctx with
                            | Matched ast          ->
                                let parsed' = CList(parsed)
                                parsed' |> collectResult ast
                                foreach parsed' cases
                            | _                    -> Unmatched
                        FindLR stackedFn'
                    | _                   -> 
                        state.Reset history
                        Unmatched
            
            foreach (CList()) composition

    | Seq(repeat, least, most)  ->
        let history = state.Commit()
        let rec foreach (parsed: AST CList) (times: int) = 
            if times = most then parsed |> Nested |> Matched
            else 
            match repeat |> Match state tokens with 
            | Matched (Nested ast) ->
                parsed.AddRange ast
                foreach parsed <| times + 1
            | Matched ast ->
                parsed.Add ast 
                foreach parsed <| times + 1
            | FindLR stackedFn ->
                let stackedFn' (ast: AST) (ctx: Context) =
                    
                    match stackedFn ast ctx with 
                    | Matched ast ->
                        let parsed' = CList(parsed)
                        parsed' |> collectResult ast
                        foreach parsed' <| times + 1
                    | _           ->
                        if times < least then Unmatched
                        else parsed |> Nested |> Matched
                FindLR stackedFn'
            | _               ->
                if times < least 
                then 
                    state.Reset history
                    Unmatched
                else parsed |> Nested |> Matched
            
        foreach (CList()) 0
        
    
            