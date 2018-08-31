module RBNF.analyse
open RBNF.ParserC
open RBNF.Lexer

type bound_name_descriptor = {
    name   : string
    is_lst : bool
}

type analysis = {
    bounds   : bound_name_descriptor Set
    lexer_tb : lexer list
}

let merge_lexer_tb tb lexer: lexer list = failwith ""
let merge_lexer_tbs (tb1: lexer list) (tb2: lexer list): lexer list =
    List.fold (fun a b -> merge_lexer_tb a b) tb1 tb2

let rec analyse (analysis: analysis) (lang: (string, 't parser) Map) =
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
        | And(many) -> List.fold proc analysis many
        | _ -> analysis
    
    let bounds, lexer_tbs = 
        [
            for (name, parser) in Map.toList lang do
                let analysis = proc analysis parser
                yield (name, analysis.bounds), analysis.lexer_tb
        ] 
        |> List.unzip
    Map.ofList bounds, List.reduce merge_lexer_tbs lexer_tbs
            
    
    