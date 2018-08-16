module RBNF.AST
open RBNF.Infras

type Token = {
    filename : string
    lineno   : int
    colno    : int
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
        let (Nested lst) = n // only one pattern kind
        in
        lst.Add(value)
