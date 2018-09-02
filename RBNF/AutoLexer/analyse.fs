module RBNF.analyse
open RBNF.ParserC
open RBNF.Lexer



let mergeable = function
    | StringFactor _, StringFactor _ -> true
    | RegexFactor l, RegexFactor r -> l.ToString() = r.ToString()
    | _ -> false

let merge_lexer_tb (tb: lexer array) (lexer: lexer): lexer array =
    Array.tryFindIndexBack
    <| fun (it: lexer)  -> it.name = lexer.name && mergeable(it.factor, lexer.factor)
    <| tb
    |>
    function
    | None ->
        Array.append tb [|lexer|]

    | Some i ->
    let var =
        match tb.[i], lexer with
        | {name=name;factor = StringFactor ls}, {factor = StringFactor rs}->
            {name=name; factor = StringFactor <| (List.distinct <| List.append ls rs)}
        | {name = name; factor = RegexFactor _}, it -> it
        | _ -> failwith "impossible"
    tb.[i] <- var
    tb

let merge_lexer_tbs (tb1: lexer array) (tb2: lexer array): lexer array =
    Array.fold merge_lexer_tb tb1 tb2

let rec inline analyse (parsers: 't parser list) =
    let rec proc analysis parser =
        match parser with
        | Literal {lexer = Some lexer} ->
            let lexer = lexer()
            merge_lexer_tb analysis lexer

        | AnyNot(parser)
        | Rewrite(parser, _)
        | Lens(_, parser)
        | Rep(_, _, parser) -> proc analysis parser

        | Or(many)
        | And(many) ->
            List.fold proc analysis many
        | _ -> analysis

    let lexer_tbs = List.map (proc [||]) parsers
    let fn (lexer : lexer) =
        match lexer with
        | {factor = StringFactor lst;} ->
            {lexer with factor = StringFactor <| List.sortDescending lst}
        | _ -> lexer
    in List.reduce merge_lexer_tbs lexer_tbs |> Array.map fn |> Array.toList
