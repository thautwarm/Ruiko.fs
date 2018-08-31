module RBNF.analyse
open RBNF.ParserC
open RBNF.Lexer
open RBNF.Infras
open System.Linq

type bound_name_descriptor = {
    name   : string
    is_lst : bool
}

type analysis = {
    bounds   : bound_name_descriptor Set
    lexer_tb : lexer array
    }
    with
    static member crate = {bounds = set []; lexer_tb = [||]}


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

let rec analyse (analysis: analysis) (lang: (string, 't parser) hashmap) =
    let lang = seq{ for a in lang -> (a.Key, a.Value)} |> Map.ofSeq
    let rec proc analysis parser =
        match parser with
        | Literal {lexer = Some lexer} -> 
            let lexer = lexer()
            {analysis with lexer_tb = merge_lexer_tb analysis.lexer_tb lexer}
            
        | AnyNot(parser)
        | Rewrite(parser, _)
        | Rep(_, _, parser) -> proc analysis parser
        | Bind(varname, parser) ->
            let record =  {name = varname; is_lst=false}
            let analysis = 
                match Set.contains record analysis.bounds with
                | false ->  
                    {analysis with bounds =  analysis.bounds.Add record}
                | true -> 
                    analysis
            proc analysis parser

        | Push(varname, parser) ->
            let record =  {name = varname; is_lst=true}
            let analysis = 
                match Set.contains record analysis.bounds with
                | false ->  
                    {analysis with bounds =  analysis.bounds.Add record}
                | true -> 
                    analysis
            proc analysis parser

        | Or(many) 
        | And(many) ->
            List.fold proc analysis many
        | _ -> analysis
    
    let bounds, lexer_tbs = 
        [
            for (name, parser) in Map.toList lang do
                let analysis = proc analysis parser
                yield (name, analysis.bounds), analysis.lexer_tb
        ] 
        |> List.unzip
    Map.ofList bounds, 
    let fn (lexer : lexer) = 
        match lexer with
        | {factor = StringFactor lst;} -> 
            {lexer with factor = StringFactor <| List.sortDescending lst}
        | _ -> lexer
    in List.reduce merge_lexer_tbs lexer_tbs |> Array.map fn
            
    
    