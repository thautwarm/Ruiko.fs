module RBNF.AST
open RBNF.Infras

type Token = {
    filename : string
    lineno   : int
    colno    : int
    offset   : int
    name     : string
    value    : string
}

type 'T AST =
    | Value  of 'T
    | MExpr  of string * 'T AST
    | Nested of 'T AST arraylist
    | Token  of Token


let push'(ctx : (string, 'T AST) hashmap)(k: string) (value: 'T AST) =
    match ctx.TryGetValue k with
    | (false, _) ->
        ctx.[k] <- Nested (arraylist [value])
    | (true, n)  ->
        let (Nested lst) = n // only one pattern here
        in
        lst.Add(value)

let merge_nested (nested: 'T AST arraylist) =
    function
    | Nested arr ->
        nested.AddRange arr
    | it ->
    nested.Add it
