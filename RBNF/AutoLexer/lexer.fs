module RBNF.Lexer
open RBNF.AST
open System.Text.RegularExpressions
open Ruikowa.CSharp
open System
open RBNF
open RBNF

type lexer_factor =
    | RegexFactor  of Regex
    | StringFactor of string list

type string_view = {
    ref    : string
    offset : int
}


let lexer_factor_match str_view =
    function
    | RegexFactor r ->
        let r = r.Match(str_view.ref, str_view.offset)
        if r.Success then r.Value |> Some
        else None
    | StringFactor ss ->
        List.tryFind
        <| fun it ->
            StrUtils.StartsWithAt(str_view.ref, it, str_view.offset)
        <| ss

type lexer_matcher = string_view -> string option

type lexer = {
    name    : string
    factor  : lexer_factor
}

type source = {
    filename : string
    text     : string
}

type lexer_table = lexer list

type cast_map = (string, string) Map
// `cast_map` is the most difficult concept in RBNF.
// when a word `w` is extracted from the view of source text,
//if there is a `cast_map`, if `cast_map` contains the key `w`,
//a token whose value is `CachingPool.cast(w)` and named `cast_map[w]` would be yielded out.

//illustration:

//lexer_table:
//[...; {name = "identifier", matcher = matcher}, ...]

//string view:
//{
//value  = "1 match ...}
//offset = 2
//}

//matcher "match..." -> Some "match"

//cast_map:
//Map [("match", "keyword"), ...]

//yield out:
//Token(name = cast_map.["match"], value = Cachingpool.cast "match", ...)


let lex (cast_map: cast_map option)
        (lexer_table: lexer_table)
        (src: source) =
    let view = {ref = src.text; offset = 0}
    let n = src.text.Length
    let mutable lineno = 0
    let mutable colno  = 0
    let filename = src.filename
    let rec loop view = seq{
        match view with
        | {ref = _; offset = offset} when offset = n ->
            ()
        | _ ->
        let picked =
            let match' = lexer_factor_match view
            List.tryPick
            <| fun {name=name; factor=factor} ->
                match match' factor with
                | None -> None
                | Some it -> Some (name, it)
            <| lexer_table
        match picked with
        | None ->
            let {ref = value; offset = offset} = view
            let sample = value.Substring(offset, offset + 15)
            failwithf "unknown string head: `%s` at line %d, column %d, file %s"
                        sample lineno colno filename
        | Some (name, word) ->
            let tk_name, tk_word =
                match cast_map with
                | Some cast_map ->
                    match Map.tryFind word cast_map with
                    | None      -> CachingPool.cast name, word
                    | Some name -> CachingPool.cast name, CachingPool.cast word
                | None -> CachingPool.cast name, word
            yield  {
                    name = tk_name
                    value = tk_word
                    offset = view.offset
                    filename = filename
                    colno = colno
                    lineno = lineno
                   }
            let word_len = String.length word
            match StrUtils.StringCount(word, '\n') with
            | 0 ->
                colno <- colno + word_len
            | line_inc ->
                lineno <- lineno + line_inc
                colno  <- word_len - StrUtils.StringFindIndexRight(word, '\n') - 1
            yield! loop({view with offset = view.offset + word_len})
        }

    in loop view

let lexer_tb_to_const lexer_tb =
    [
        for each in lexer_tb ->
        match each with
        |{factor = StringFactor lst; name = name} ->
            {name = CachingPool.cast name; factor = StringFactor <| List.map CachingPool.cast lst}
        | {name = name} -> {each with name = CachingPool.cast name}
    ]
