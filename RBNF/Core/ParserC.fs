﻿module RBNF.ParserC
open RBNF.Infras
open RBNF.AST
open Ruikowa.CSharp
open System

type 't parser =
| Predicate of ('t state -> bool)
| Rewrite   of ('t parser * ('t state -> 't AST -> 't AST))
(**literal*)
| Literal   of (Token -> bool)
| Any
(**atom*)
| Bind  of string * 't parser
| Push  of string * 't parser
| Named of string

(** composed *)
| And    of 't parser list
| Or     of 't parser list
| Rep    of int * int * 't parser
// | Jump   of (string, Parser) Map
| AnyNot of 't parser

and 't state = {
    mutable lr    : string option
    mutable ctx   : (string, 't AST) hashmap
    trace : string Trace Trace
    lang  : (string, 't parser) hashmap
    }
    with
    static member inst(): 't state = 
        let trace = Trace()
        trace.Append(Trace())
        {
            lr = None
            ctx = hashmap()
            trace = trace
            lang = hashmap()
        }

    member this.end_index with get() = this.trace.EndIndex
    member this.max_fetched with get() = this.trace.MaxFetched
    member this.current with get () = this.trace.[this.trace.EndIndex]
    member this.reset(history) =
            let (base', branch, ctx) = history
            this.ctx <- ctx
            this.trace.Reset(base')
            this.current.Reset(branch)

    member this.new_one() =
        if this.trace.Inc(fun () -> Trace<string>()) = false then
            this.current.Clear()
        else
            ()

    member this.append(e : string) =
        this.current.Append(e)
    member this.commit() =
        (this.trace.Commit(), this.current.Commit(), this.ctx)

    member this.contains(record : string) =
        this.current.Contains(record)

    static member left_recur (self : 't state) (lr_name : string) (fn : 't state -> 'r) : 'r =
        Log(fun() -> sprintf "start lr for %A" lr_name)
        self.lr <- Some lr_name
        let ret = fn(self)
        self.lr <- None
        Log(fun () -> sprintf "end lr for %A" lr_name)
        ret
    
    static member with_context_recovery (self: 't state) (fn: 't state -> 'r): 'r = 
        let ctx = self.ctx
        let ret = fn(self)
        self.ctx <- ctx
        ret

and 't State = 't state

type 't Result =
   | Unmatched
   | Matched of 't AST
   | LR      of parser: 't parser * (('t Result) -> 't Result)

let rec parse (self : 't parser)
              (tokens : Token array)
              (state : 't state): 't Result =
    match self with
    | Rewrite(parser, app) ->
        match parse parser tokens state with
        | Unmatched -> Unmatched
        | Matched v -> Matched <| app state v
        | LR(_, stack') ->
        let stack(res: 't Result) = 
            match stack' res with 
            | LR _ | Unmatched -> Unmatched
            | Matched v -> Matched <| app state v
        LR(self, stack)

    | Predicate pred ->
        if pred state then 
            Unchecked.defaultof<'t> |> Value |> Matched
        else
            Unmatched
    | Any ->
        let idx = state.end_index
        if tokens.Length <= idx then
            Unmatched
        else
        let token = tokens.[idx]
        state.new_one() |> ignore
        Matched <| Token token
    | Literal lit ->
        let idx = state.end_index
        if tokens.Length <= idx then
            Unmatched
        else
        let token = tokens.[idx]
        in
        if lit token then
            state.new_one() |> ignore
            Matched(Token token)
        else 
            Unmatched


    | Bind(name, parser) ->
        match parse parser tokens state with
        | Unmatched ->
            Unmatched
        | (Matched result) as it ->
            state.ctx.[name] <- result
            it
        | LR (pobj, stack') as it ->
            let stack(ast : 't Result) =
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
            let stack(ast : 't Result) =
                match stack' ast  with
                | Unmatched -> Unmatched
                | Matched v as it ->
                    push' state.ctx name v
                    it
                | _ -> failwith "Impossible"
            LR(self, stack)
        
    | Named name ->
        Log <| fun () -> "start " + name
        let parser = state.lang.[name]
        let inline exit_task(v) =
            Matched <| MExpr(name, v)
        if state.contains name then
            Log <| fun() -> sprintf "state contains %A" name
            if state.lr <> None then
                Log <| fun () -> sprintf "match failed %A" name
                Unmatched
            else
            state.lr <- Some name
            Log <| fun () -> sprintf "assign lr name: %A" name
            let stack (res: 't Result) =
                res
            LR(self, stack)
        else
        Log <| fun () -> sprintf "state doesn't contain %A" name
        State<'t>.with_context_recovery state <|
        fun state ->
        state.append name
        state.ctx <- hashmap()
        match parse parser tokens state with
        | Unmatched -> Unmatched | Matched v -> exit_task v 
        | LR(pobj, stack') ->
        if name <> state.lr.Value then
            let stack(ast: 't Result) =
                match stack' ast with
                | Unmatched | LR _ -> Unmatched
                | Matched v        -> exit_task v
            LR(self, stack)
        else
        State<'t>.left_recur state name <| 
        fun state ->
        let ctx = hashmap(state.ctx) // copy
        match parse parser tokens state with
        | LR _ | Unmatched -> Unmatched
        | Matched head     ->
        let rec loop head =
            let head = exit_task head
            Log <| fun() -> sprintf "loop++, head : %A" head
            match State<'t>.with_context_recovery state 
                  <| fun state ->
                     state.ctx <- hashmap(ctx)
                     stack' head
                    with
            | Unmatched | LR _ -> head
            | Matched recur    -> loop recur
        loop head
    | AnyNot comp ->
        let history = state.commit()
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
        let history = state.commit()
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
        let history = state.commit()
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
        let history = state.commit()
        let nested     = arraylist()
        let FINISH     = 0
        let CONTINUE   = 1
        let FAIL       = 2
        let FINDLR     = 3
        let mutable lr = None 
        let foreach (nested: 't AST arraylist) times =
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
            | LR(pobj, stack') ->
                if at_least = 0 then
                    state.reset sub_history
                    let msg = 
                        sprintf
                            "Left recursion supporting is ambiguous with repeatable parser(%A) that which couldn't fail." 
                            self
                    System.Diagnostics.Trace.TraceWarning msg
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


//match lit with
//            | Any     -> true
//            | C c_str -> c_str &= token.value
//            | N name  -> name  &= token.name
//            | V value -> value = token.value
//            | NC(name, c_str) -> name &= token.name && c_str &= token.value
//let C string = cast string |> C |> Literal 
//let V string = string |> V |> Literal 
//let NC name string = NC(cast <| name, cast <| string) |> Literal 
//let N name         = N(cast <| name) |> Literal


//let And ands = ands |> And |> Composed 
//let Or  ors  = ors |> Or |> Composed 
//let Rep at_least at_most parser = Rep(at_least, at_most, parser) |> Composed 
//let Bind name parser = Bind(name, parser) |> Atom 
//let Push name parser = Push(name, parser) |> Atom 
//let Named name = Named(name) |> Atom 