module RBNF.Operator
open RBNF.Infras
open RBNF.AST
open RBNF.ParserC

//match lit with
//        | Any     -> true
//        | C c_str -> c_str &= token.value
//        | N name  -> name  &= token.name
//        | V value -> value = token.value
//        | NC(name, c_str) -> name &= token.name && c_str &= token.value

let C string =
    let string = CachingPool.cast string
    fun (token: Token) -> token.name &= string
    |> Literal
 
let N name =
    let name = CachingPool.cast name
    fun (token: Token) -> token.name &= name
    |> Literal
    
let V value =
    fun (token: Token) -> token.value = value
    |> Literal

let NC name value =
    let value = CachingPool.cast value
    let name   = CachingPool.cast name
    fun (token: Token) -> token.name &= name && token.value &= value
    |> Literal

let NV name value =
    let name   = CachingPool.cast name
    fun (token: Token) -> token.name &= name && token.value = value
    |> Literal