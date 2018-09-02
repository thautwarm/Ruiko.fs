module RBNF.ParserC
open RBNF.Infras
open RBNF.AST
open Ruikowa.CSharp
open System
open RBNF.Lexer

type literal = {
    test  : Token -> bool
    lexer : (unit -> lexer) option
}

type 't parser =
    | Predicate of ('t state -> bool)
    | Rewrite   of  't parser * 't rewrite
    (**literal*)
    | Literal   of literal
    | Any
    (**atom*)
    | Lens  of ('t -> 't AST -> 't) * 't parser
    | Named of string * (unit -> 't)

    (** composed *)
    | And    of 't parser list
    | Or     of 't parser list
    | Rep    of int * int * 't parser
    // | Jump   of (string, Parser) Map
    | AnyNot of 't parser

    static member inline (=>) (this, fn) =
        Rewrite(this, fn)

    static member inline (!) this =
        AnyNot this
    
    static member inline (%) (this, lens) = 
        Lens(lens, this)

    member inline this.otherwise(other) =
        match this with
        | Or this ->
            match other with
            | Or other -> List.append this other
            | _        -> List.append this [other]
        | _ ->
            match other with
            | Or other -> this :: other
            | _        -> [this; other]
        |> Or

    static member inline (|||) (this: 't parser, other) =
        this.otherwise(other)

    member inline this.next_by(other) =
        match this with
        | And this ->
            match other with
            | And other -> List.append this other
            | _        -> List.append this [other]
        | _ ->
            match other with
            | And other -> this :: other
            | _        -> [this; other]
        |> And

    static member inline(?) (this) =
        Rep(0, 1, this)

    member inline this.repeat(at_least: int) = Rep(at_least, -1, this)

    member inline this.repeat(at_least: int, at_most: int) = Rep(at_least, at_most, this)

    member inline this.join(p: 't parser) = And [this; Rep(0, -1, And [p; this])]

    member inline this.Item
        with get(at_least: int, at_most: int) = Rep(at_least, at_most, this)



and 't rewrite = 't state -> 't AST

and 't state = {
    mutable lr     : (int, string) hashmap
    mutable context   : 't
    trace : string Trace Trace
    lang  : (string, 't parser) hashmap
    }
    with
    static member inline inst(top: 't): 't state =
        let trace = Trace()
        trace.Append(Trace())
        {
            lr = hashmap()
            context = top
            trace = trace
            lang = hashmap()
        }

    static member inline inst(): 't state =
        let trace = Trace()
        trace.Append(Trace())
        {
            lr = hashmap()
            context = Unchecked.defaultof<'t>
            trace = trace
            lang = hashmap()
        }

    member inline this.end_index with get() = this.trace.EndIndex
    member inline this.max_fetched with get() = this.trace.MaxFetched
    member inline this.current with get () = this.trace.[this.trace.EndIndex]
    member inline this.reset(history) =
            let (base', branch, ctx) = history
            this.context <- ctx
            this.trace.Reset(base')
            this.current.Reset(branch)

    member inline this.new_one() =
        if this.trace.Inc(fun () -> Trace<string>()) = false then
            this.current.Clear()
        else
            ()

    member inline this.append(e : string) =
        this.current.Append(e)
    member inline this.commit() =
        (this.trace.Commit(), this.current.Commit(), this.context)

    member inline this.contains(record : string) =
        this.current.Contains(record)

    static member inline left_recur (self : 't state) (lr_idx : int) (fn : 't state -> 'r) : 'r =
        Log(fun() -> sprintf "start lr %A." self.lr)
        let ret = fn(self)
        self.lr.Remove lr_idx |> ignore
        Log(fun () -> sprintf "end lr %A." self.lr)
        ret

    static member inline with_context_recovery (self: 't state) (fn: 't state -> 'r): 'r =
        let ctx = self.context
        let ret = fn(self)
        self.context <- ctx
        ret

and 't State = 't state

and 't Result =
   | Unmatched
   | Matched of 't AST
   | LR      of parser: string * (('t Result) -> 't Result)

let rec parse (self : 't parser)
              (tokens : Token array)
              (state : 't state): 't Result =
    match self with
    | Rewrite(parser, app) ->
        match parse parser tokens state with
        | Unmatched -> Unmatched
        | Matched _ -> Matched <| app state
        | LR(pobj, stack') ->
        let stack(res: 't Result) =
            match stack' res with
            | LR _ | Unmatched -> Unmatched
            | Matched v -> Matched <| app state
        LR(pobj, stack)

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
        if lit.test token then
            state.new_one() |> ignore
            Matched(Token token)
        else
            Unmatched

    | Lens(lens, parser) ->
        match parse parser tokens state with
        | Unmatched ->
            Unmatched
        | Matched result as it ->
            state.context <- lens state.context result
            it
        | LR (pobj, stack') ->
            let stack(ast : 't Result) =
                match stack' ast with
                | Unmatched -> Unmatched
                | Matched v as it ->
                    state.context <- lens state.context v
                    it
                | _ -> failwith "Impossible"
            LR(pobj, stack)

    | Named(name, cons) ->
        Log <| fun () -> "start " + name
        let parser = state.lang.[name]
        let exit_task =
            match parser with
            | Rewrite _ ->
                let inline exit_task v =
                    Matched v
                exit_task
            | _ ->
                let inline exit_task v =
                    Matched <| MExpr(name, v)
                exit_task

        if state.contains name then
            Log <| fun() -> sprintf "state contains %A" name
            if state.lr.ContainsKey state.end_index  then
                Log <| fun () -> sprintf "match failed %A" name
                Unmatched
            else
            state.lr.Add (state.end_index, name)
            Log <| fun () -> sprintf "assign lr name: %A" name
            let stack (res: 't Result) =
                res
            LR(name, stack)
        else
        Log <| fun () -> sprintf "state doesn't contain %A" name
        State<'t>.with_context_recovery state <|
        fun state ->
        state.append name
        state.context <- cons()
        match parse parser tokens state with
        | Unmatched -> Unmatched | Matched v -> exit_task v
        | LR(pobj, stack') ->
        if pobj <> name then
            let stack(ast: 't Result) =
                match stack' ast with
                | Unmatched | LR _ -> Unmatched
                | Matched v        -> exit_task v
            LR(pobj, stack)
        else
        State<'t>.left_recur state state.end_index <|
        fun state ->
        Log <| fun() -> sprintf "lr for : %s %d" name state.end_index
        let ctx = state.context // copy
        match parse parser tokens state with
        | LR _ | Unmatched -> Unmatched
        | Matched head     ->
        let rec loop head =
            let head = exit_task head
            Log <| fun() -> sprintf "loop++, head : %A" head
            match State<'t>.with_context_recovery state
                  <| fun state ->
                     state.context <- ctx
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
            | LR(pobj, stack') ->
            let stack ast =
                match stack' ast with
                | Unmatched | LR _ -> loop left
                | Matched _ as it  -> it
            LR(pobj, stack)
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
                merge_nested nested v
                loop is_lr nested left
            | LR(pobj, stack') ->
            if is_lr then Unmatched
            else
            let stack ast =
                let nested' = arraylist(nested)
                match stack' ast with
                | Unmatched | LR _ -> Unmatched
                | Matched v        ->
                merge_nested nested' v
                loop true nested' left
            LR(pobj, stack)
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
            | Matched v ->
                merge_nested nested v
                CONTINUE

        let rec loop_lr nested times =
            match foreach nested times with
            | x when x = CONTINUE    -> loop_lr nested <| times + 1
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
                    merge_nested nested v
                    loop_lr nested <| times + 1
                LR(pobj, stack)
            | _ -> failwith "Lack of DT"

        loop_no_lr nested 0
