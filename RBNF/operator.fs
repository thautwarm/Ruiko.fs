module RBNF.Operator
open RBNF.Infras
open RBNF.AST
open RBNF.ParserC
open RBNF.Lexer
open System.Text.RegularExpressions

//match lit with
//        | Any     -> true
//        | C c_str -> c_str &= token.value
//        | N name  -> name  &= token.name
//        | V value -> value = token.value
//        | NC(name, c_str) -> name &= token.name && c_str &= token.value

let inline C string =
    let string = CachingPool.cast string
    {test  = fun (token: Token) -> token.value &= string
     lexer = Some <| fun () -> {name = CachingPool.cast "auto_const"; factor = StringFactor [string]}}
    |> Literal

let inline N name =
    let name = CachingPool.cast name
    {test  = fun (token: Token) -> token.name &= name
     lexer = None}
    |> Literal

let inline V value =
    {test = fun (token: Token) -> token.value = value
     lexer = None}
    |> Literal

let inline NC name value =
    let value = CachingPool.cast value
    let name   = CachingPool.cast name
    {test = fun (token: Token) -> token.name &= name && token.value &= value
     lexer = Some <| fun () -> {name = name; factor = StringFactor [value]}}
    |> Literal

let inline NV name value =
    let name   = CachingPool.cast name
    {
     test = fun (token: Token) -> token.name &= name && token.value = value
     lexer = None
    }
    |> Literal

let inline R name regex =
    let regex = Regex <| "\G" + regex
    let name   = CachingPool.cast name
    let factor = RegexFactor regex
    {
     test = fun (token: Token) -> token.name &= name
     lexer = Some <| fun () -> {name = name; factor = factor}
    }
    |> Literal

type 't state with
    member inline this.implement (named: 't parser) (parser : 't parser) =
        match named with
        | Named(name, _) ->
            this.lang.[name] <- parser
        | _ -> failwith "Only named parser can be implemented."
