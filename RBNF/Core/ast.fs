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

let merge_nested (nested: 'T AST arraylist) =
    function
    | Nested arr ->
        nested.AddRange arr
    | it ->
    nested.Add it
