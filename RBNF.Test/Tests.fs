module Tests

open System
open Xunit
open RBNF.Infras
open RBNF.AST
open RBNF.State
open RBNF.ParserC
open RBNF.Lexer
open Xunit.Abstractions
open RBNF

let def_token str_lst = 
    str_lst 
    |> Array.ofList
    |> Array.map 
       (fun str ->
            {filename = ""; value = str ; name = "const" ; colno = 1; lineno = 1})

type MyTests(output:ITestOutputHelper) =

    [<Fact>]
    member __.``Direct Left Recursion`` () =
        
        let v1 = V("123") 
        let v2 = V("234") 
        
        let node = Named("node")
        
        let node_imp = Or([And([node; v1]); v2])

        let tokens = def_token ["234"; "123"; "123"]
        let take_or = Or([And([v2; v1]); v2])

        let state = State<string>.inst()
        state.lang.["node"] <- GuardRewriter<string>.wrap(node_imp)
        parse node tokens state |> sprintf "%A" |> output.WriteLine

        0

    [<Fact>]
    member __.``Indirect Left Recursion`` () =
        
        let v1 = V("a") 
        let v2 = V("b") 
        let v3 = V("c")
        let node = Named("node")
        let mid = Named("mid")

        let node_imp = Or [And [mid; v2]; v1]
        let mid_imp = And [node; v3]

        let tokens = def_token ["a"; "c"; "b"; "c"; "b"]

        let state: State<string> = State<string>.inst()
        state.lang.["node"] <- GuardRewriter<string>.wrap(node_imp)
        state.lang.["mid"]  <- GuardRewriter<string>.wrap(mid_imp)
        let raise' b = Assert.True(false, b)

        match parse node tokens state with 
        | Matched(MExpr("node", Nested lst)) ->
            let a, b = lst.[0], lst.[1]
            match b with
            | Token {value = "b"} -> 
                match a with 
                | MExpr("mid", Nested lst) -> 
                    let a, b = lst.[0], lst.[1]
                    match b with
                    | Token {value = "c"} ->
                        ()
                    | _ as it -> raise' <| sprintf "4 %A" it 
                | _ as it ->  raise' <| sprintf "3 %A" it 
            | _  as it ->  raise' <| sprintf "2 %A" it 
        | _ as it -> raise' <| sprintf "1 %A" it 
        0
    [<Fact>]
    member __.``auto lexer``() =
        let factor = StringFactor ["123"; "aaa"; "*&^"]
        let lexer_tb = [{factor = factor; name=CachingPool.cast "const"}]
        let cast_map = None
        
        lex cast_map lexer_tb {text = "123aaa*&^"; filename = "a.fs"}
        |> List.ofSeq
        |> sprintf "%A"
        |> output.WriteLine

        0
 

    

    