module RBNF.ParserC

open RBNF.Infras
open RBNF.AST
open RBNF.State
open RBNF.CachingPool

open System.Text.RegularExpressions
open System.Globalization
open Ruikowa.CSharp
open System.Collections
open System.Diagnostics
open System.Diagnostics

type Literal =
    | C  of string
    | N  of string
    | V  of string
    // | R  of Regex
    | NC of string * string
    | Any

and Atom =
    | Bind  of string * Parser
    | Push  of string * Parser
    | Named of string

and Composed =
    | And    of Parser list
    | Or     of Parser list
    | Rep    of int * int * Parser
    // | Jump   of (string, Parser) Map
    | AnyNot of Parser

and Parser =
    | Literal  of Literal
    | Atom     of Atom
    | Composed of Composed

type 'V Result =
   | Unmatched
   | Matched of 'V
   | LR      of pobj: obj * (('V Result) -> 'V Result)


let rec parse (self : Parser)
              (tokens : Token array)
              (state : (Parser, 'T) State): 'T AST Result =
    match self with
    | Literal lit ->
        let idx = state.end_index
        if tokens.Length <= idx then
            Unmatched
        else
        let token = tokens.[idx]
        if
            match lit with
            | Any     -> true
            | C c_str -> c_str &= token.value
            | N name  -> name  &= token.name
            | V value -> value = token.value
            | NC(name, c_str) -> name &= token.name && c_str &= token.value
        then
            state.new_one() |> ignore
            Matched(Token token)
        else
            Unmatched

    | Atom atom ->
        match atom with
        | Bind(name, parser) ->
            match parse parser tokens state with
            | Unmatched ->
                Unmatched
            | (Matched result) as it ->
                state.ctx.[name] <- result
                it
            | LR (pobj, stack') as it ->
                let stack(ast : 'T AST Result) =
                    match stack' ast with
                    | Unmatched -> Unmatched
                    | Matched v as it ->
                        state.ctx.[name] <- v
                        it
                    | _ -> failwith "Impossible"
                LR(self, stack)

        | Push(name, parser) ->
            match parse parser tokens state with
            | Unmatched ->
                Unmatched
            | (Matched result) as it ->
                push' state.ctx name result 
                it
            | LR (pobj, stack') as it ->
                let stack(ast : 'T AST Result) =
                    match stack' ast  with
                    | Unmatched -> Unmatched
                    | Matched v as it ->
                        push' state.ctx name v
                        it
                    | _ -> failwith "Impossible"
                LR(self, stack)
        | Named name ->
            Log("start " + name)
            let {
                parser  = parser
                enter   = enter
                exit    = exit
                rewrite = rewrite
                } = state.lang.[name]
            in
            let exit_task(v) =
                exit |>
                function
                | Some exit when not <| exit tokens state -> Unmatched
                | _ ->
                Matched <|
                match rewrite with
                | Some rewrite -> Value <| rewrite state
                | _ -> MExpr(name, v)

            enter |>
            function
            | Some enter when not <| enter tokens state -> Unmatched
            | _ ->

            if state.contains name then
                Log(sprintf "state contains %A" name)

                if state.lr <> None then
                    Log(sprintf "match failed %A" name)
                    Unmatched
                else
                Log(sprintf "assign lr name: %A" name)
                state.lr <- Some name
                let stack (ast : 'T AST Result) =
                    ast

                LR(self, stack)
            else
            Log(sprintf "state doesn't contain %A" name)
            with_context_recovery state <|
            fun state ->
            state.append name
            state.ctx <- hashmap()
            match parse parser tokens state with
            | Unmatched -> Unmatched
            | Matched v -> exit_task(v)
            | LR(pobj, stack') ->
            if state.lr.Value <> name then
                let stack(ast: 'T AST Result) =
                    match stack' ast with
                    | Unmatched
                    | LR _      -> Unmatched
                    | Matched v -> exit_task(v)
                LR(self, stack)
            else
            left_recur state name <| 
            fun state ->
            let ctx = hashmap(state.ctx) // copy
            match parse parser tokens state with
            | LR _ | Unmatched -> Unmatched
            | Matched head     ->
            let rec loop head =
                Log <| sprintf "loop++, head : %A" head
                match exit_task(head) with
                | Unmatched | LR _  -> Unmatched
                | Matched _ as recur ->
                Log <| sprintf "recur: %A" recur
                match with_context_recovery state <|
                        fun state ->
                        state.ctx <- hashmap(ctx)
                        stack' recur
                        with
                | Unmatched | LR _ -> recur
                | Matched recur     -> loop recur
            loop head
    | Composed comp ->
        let history = state.commit()
        match comp with
        | AnyNot comp ->
            match parse comp tokens state with
            | Unmatched ->
                state.reset history
                let idx = state.end_index
                if tokens.Length <= idx then
                    Unmatched
                else
                    Matched <| (Token tokens.[idx])
            | _         ->
            state.reset history
            Unmatched
        | Or orz ->
            let rec loop (left) =
                match left with
                | [] -> Unmatched
                | parser :: left ->
                match parse parser tokens state with
                | Unmatched  ->
                    state.reset history
                    loop left
                | LR(pobj, _) when pobj &= self ->
                    state.reset history
                    loop left
                | Matched _ as it ->
                    it
                | LR(_, stack') ->
                let stack ast =
                    match stack' ast with
                    | Unmatched | LR _ -> loop left
                    | Matched _ as it  -> it
                LR(self, stack)
            loop orz
        | And ands ->
            let nested = arraylist()
            let rec loop (is_lr) (nested) (left) =
                match left with
                | [] -> Matched <| Nested nested
                | parser :: left ->
                match parse parser tokens state with
                | Unmatched ->
                    if not is_lr then
                        state.reset history
                    else ()
                    Unmatched
                | Matched v ->
                    nested.Add v
                    loop is_lr nested left
                | LR(_, stack') ->
                if is_lr then Unmatched
                else 
                let stack ast =
                    let nested' = arraylist(nested)
                    match stack' ast with
                    | Unmatched | LR _ -> Unmatched
                    | Matched v        ->
                    nested'.Add v
                    loop true nested' left
                LR(self, stack)
            loop false nested ands
        
        | Rep (at_least, at_most, parser)->
            let nested     = arraylist()
            let FINISH     = 0
            let CONTINUE   = 1
            let FAIL       = 2
            let FINDLR     = 3
            let mutable lr = None 
            let foreach (nested: 'T AST arraylist) times =
                if times = at_most then
                    FINISH 
                else
                let sub_history = state.commit()
                match parse parser tokens state with
                | Unmatched ->
                    if times >= at_least
                    then
                        state.reset sub_history
                        FINISH 
                    else FAIL 
                | LR(pobj, stack') as it ->
                    if at_least = 0 then
                        state.reset sub_history
                        let msg = 
                            sprintf
                              "Left recursion supporting is ambiguous with repeatable parser(%A) that which couldn't fail." 
                              self
                        Trace.TraceWarning msg
                        FAIL 
                    else
                        lr <- Some((pobj, stack'))
                        FINDLR
                | Matched (Nested lst) ->
                    nested.AddRange(lst)
                    CONTINUE

                | Matched it ->
                    nested.Add(it)
                    CONTINUE
         
            
            let rec loop_lr nested times = 
                match foreach nested times with 
                | x when x = CONTINUE    -> loop_lr nested times 
                | x when x = FINISH      -> Matched <| Nested nested 
                | x when x = FINDLR ||  x = FAIL -> Unmatched
                | _           -> failwith "Lack of DT"

            let rec loop_no_lr nested times = 
                match foreach nested times with 
                | x when x = CONTINUE -> loop_no_lr nested <| times + 1
                | x when x = FINISH   -> Matched <| Nested nested
                | x when x = FAIL     -> 
                    state.reset history
                    Unmatched
                | x when x = FINDLR   -> 
                    let (pobj, stack') = lr.Value in
                    let stack ast = 
                        let nested = arraylist nested
                        match stack' ast with 
                        | LR _ 
                        | Unmatched  -> 
                            if times < at_least then
                                Unmatched
                            else 
                            Matched <| Nested nested 
                        | Matched v ->
                        match v with
                        | Nested lst -> nested.AddRange(lst)
                        | _          -> nested.Add v 
                        loop_lr nested <| times + 1
                    LR(pobj, stack)
                | _ -> failwith "Lack of DT"

            loop_no_lr nested 0 

                
let C string = cast string |> C |> Literal 
let V string = string |> V |> Literal 
let NC name string = NC(cast <| name, cast <| string) |> Literal 
let N name         = N(cast <| name) |> Literal


let And ands = ands |> And |> Composed 
let Or  ors  = ors |> Or |> Composed 
let Rep at_least at_most parser = Rep(at_least, at_most, parser) |> Composed 
let Bind name parser = Bind(name, parser) |> Atom 
let Push name parser = Push(name, parser) |> Atom 
let Named name = Named(name) |> Atom 

type 'T State = State<Parser, 'T>
type 'T GuardRewriter = GuardRewriter<Parser, 'T>

                



                     
                       
                        
