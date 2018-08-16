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
    | Rep    of int * int * Parser list
    | Jump   of (string, Parser) Map
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
                if pobj &= self then
                    Unmatched
                else
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
                state.ctx.[name] <- result
                it
            | LR (pobj, stack') as it ->
                if pobj &= self then
                    Unmatched
                else
                let stack(ast : 'T AST Result) =
                    match stack' ast  with
                    | Unmatched -> Unmatched
                    | Matched v as it ->
                        state.ctx.[name] <- v
                        it
                    | _ -> failwith "Impossible"
                LR(self, stack)
        | Named name ->
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
                match parse parser tokens state with
                | Unmatched -> Unmatched
                | Matched v -> exit_task(v)
                | LR(pobj, stack') ->
                if not <| (pobj &= self) then
                    let stack(ast: 'T AST Result) =
                        match stack' ast with
                        | Unmatched
                        | LR _      -> Unmatched
                        | Matched v -> exit_task(v)
                    LR(self, stack)
                else
                if state.lr &= name then 
                    Unmatched
                else
                left_recur state name <|
                fun state ->
                    let ctx = hashmap(state.ctx) // copy
                    match parse parser tokens state with
                    | LR _ | Unmatched -> Unmatched
                    | Matched head     ->
                    let rec loop head =
                        match exit_task(head) with
                        | Unmatched | LR _  -> Unmatched
                        | Matched _ as head ->
                        with_context_recovery state <|
                        fun state ->
                            state.ctx <- hashmap(ctx)
                            match stack' head with
                            | Unmatched | LR _ -> head
                            | Matched head     ->
                            loop head
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
            let rec loop (nested) (left) =
                match left with
                | [] -> Matched <| Nested nested
                | parser :: left ->
                match parse parser tokens state with
                | Unmatched ->
                    state.reset history
                    Unmatched
                | LR(pobj, _) when pobj &= self ->
                    state.reset history
                    Unmatched
                | Matched v ->
                    nested.Add v
                    loop nested left
                | LR(_, stack') ->
                let stack ast =
                    let nested' = arraylist(nested)
                    match stack' ast with
                    | Unmatched | LR _ -> Unmatched
                    | Matched v        ->
                    nested'.Add v
                    loop nested' left
                LR(self, stack)
            loop nested ands
        | Seq seq ->
            let 
