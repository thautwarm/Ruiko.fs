module RBNF.Operator
open RBNF.Infras
open RBNF.AST
open RBNF.ParserC
open RBNF.Lexer

//match lit with
//        | Any     -> true
//        | C c_str -> c_str &= token.value
//        | N name  -> name  &= token.name
//        | V value -> value = token.value
//        | NC(name, c_str) -> name &= token.name && c_str &= token.value

let C string =
    let string = CachingPool.cast string
    {test  = fun (token: Token) -> token.name &= string
     lexer = Some <| fun () -> {name = CachingPool.cast "auto_const"; factor = StringFactor [string]}}
    |> Literal
 
let N name =
    let name = CachingPool.cast name
    {test  = fun (token: Token) -> token.name &= name
     lexer = None}
    |> Literal
    
let V value =
    {test = fun (token: Token) -> token.value = value
     lexer = None}
    |> Literal

let NC name value =
    let value = CachingPool.cast value
    let name   = CachingPool.cast name
    {test = fun (token: Token) -> token.name &= name && token.value &= value
     lexer = Some <| fun () -> {name = name; factor = StringFactor [value]}}
    |> Literal

let NV name value =
    let name   = CachingPool.cast name
    {test = fun (token: Token) -> token.name &= name && token.value = value
     lexer = None}
    |> Literal

type 't state with
    member this.implement (named: 't parser) (parser : 't parser) = 
        match named with
        | Named name -> 
            this.lang.[name] <- parser 
        | _ -> failwith "Only named parser can be implemented."