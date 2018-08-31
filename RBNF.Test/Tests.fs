module Tests

open System
open Xunit
open RBNF.Infras
open RBNF.AST
open RBNF.ParserC
open RBNF.Operator
open RBNF.Lexer
open Xunit.Abstractions
open RBNF
open System.Text.RegularExpressions

let def_token str_lst =
    str_lst
    |> Array.ofList
    |> Array.map
       (fun str ->
            {filename = ""; value = str ; name = "const" ; colno = 1; lineno = 1; offset = 1;})


type Expr =
| Add of Expr * Expr
| Sym of string

type MyTests(output:ITestOutputHelper) =

    [<Fact>]
    member __.``Direct Left Recursion`` () =

        let v1 = V "123"
        let v2 = V "234"
        let node_name = "node"
        let node = Named node_name
        let node_impl = Or [And [node; v1]; v2]
        let tokens = def_token ["234"; "123"; "123"]
        let state = State<string>.inst()
        let (:=) = state.implement 
        node := node_impl
        parse node tokens state |> sprintf "%A" |> output.WriteLine
        0

    [<Fact>]
    member __.``Indirect Left Recursion`` () =

        let v1 = V("a")
        let v2 = V("b")
        let v3 = V("c")
        let node = Named("node")
        let mid = Named("mid")

        let node_impl = Or [And [mid; v2]; v1]
        let mid_impl = And [node; v3]
        let tokens = def_token ["a"; "c"; "b"; "c"; "b"]

        let state: State<string> = State<string>.inst()
        let (:=) = state.implement
        node := node_impl
        mid  := mid_impl
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
    member __.``Infix Left Recursion``() =
        let identifier = V "abs"
        let plus_operator = V "+"
        let plus_name = "plus"
        let plus = Named plus_name
        let plus_impl = Or [And [plus; plus_operator; identifier]; identifier]

        let tokens = 
            def_token <| 
            [
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
            ]
        let state = State<string>.inst()
        let (:=) = state.implement
        plus := plus_impl
        parse plus tokens state |> sprintf "%A" |> output.WriteLine

        0

    [<Fact>]
    member __.``auto lexer preview``() =
        let factor = StringFactor ["123"; "aaa"; "*&^"]
        let lexer_tb = [{factor = factor; name=CachingPool.cast "const"}]
        let cast_map = None

        lex cast_map lexer_tb {text = "123aaa*&^"; filename = "a.fs"}
        |> List.ofSeq
        |> sprintf "%A"
        |> output.WriteLine
        0

    [<Fact>]
    member __.``auto lexer``() =
        let identifier = Regex "\G[a-zA-Z_]{1}[a-zA-Z_0-9]*"
        let space = Regex "\G\s+"
        let identifier = RegexFactor identifier
        let space = RegexFactor space
        let lexer_tb = [
            {factor = identifier; name = CachingPool.cast "regex"}
            {factor = space     ; name = CachingPool.cast "space"}
        ]
        let cast_map = None
        lex cast_map lexer_tb  {text = "I am the bone of my sword"; filename = "a.fs"}
        |> List.ofSeq
        |> sprintf "%A"
        |> output.WriteLine
        0

    [<Fact>]
    member __.``rewrite add``() =
        let identifier = V "abs"
        let plus_operator = V "+"
        let plus_name = "plus"
        let plus = Named plus_name
        let tokens = 
            def_token <| 
            [
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
                "+"
                "abs"
            ]
        let state = State<Expr>.inst()
        
        let (:=) = state.implement
        let identifier = 
            identifier =>
            fun state ->
            function
            | Token it -> Value <| Sym(it.value)
            | _ as it -> failwithf "%A" it

        plus := Or
                [ 
                    And [plus; plus_operator; identifier]
                    identifier
                ]
                =>
                fun state ast ->
                match ast with
                | Nested arr ->
                    let (Value l) = arr.[0]
                    let (Value r) = arr.[2]
                    Add(l, r) |> Value
                | _ -> ast
        
        sprintf "%A" state.lang.["plus"] |> output.WriteLine

        parse plus tokens state |> sprintf "%A" |> output.WriteLine

        0