module Tests

open System
open Xunit
open RBNF.Infras
open RBNF.AST
open RBNF.ParserC
open RBNF.Operator
open RBNF.Lexer
open Xunit.Abstractions
open RBNF.analyse
open RBNF.CachingPool
open System.Text.RegularExpressions


let def_token str_lst =
    str_lst
    |> Array.ofList
    |> Array.map
       (fun str ->
            {filename = ""; value = str ; name = "const" ; colno = 1; lineno = 1; offset = 1;})

type sexpr =
| Term of string
| S    of sexpr list

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
        let lexer_tb = [{factor = factor; name=cast "const"}]
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
            {factor = identifier; name = cast "regex"}
            {factor = space     ; name = cast "space"}
        ]
        let cast_map = None
        lex cast_map lexer_tb  {text = "I am the bone of my sword"; filename = "a.fs"}
        |> List.ofSeq
        |> sprintf "%A"
        |> output.WriteLine
        0

    [<Fact>]
    member __.``rewrite add``() =
        let state = State<Expr>.inst()
        let plus = Named "plus"

        let (:=) = state.implement
        let identifier =
            R "identifier" "[a-zA-Z_]{1}[a-zA-Z_0-9]*" =>
            fun state ->
            function
            | Token it -> Value <| Sym(it.value)
            | _ as it -> failwithf "%A" it

        plus := Or
                [
                    And [plus.bind_to("emm");  C "+"; identifier]
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
        let a, b = analyse analysis.crate [for each in ["plus"] -> each, state.lang.[each]]


        let tokens = lex None b {filename=""; text="abs+abs+abs"} |> Array.ofSeq

        parse plus tokens state |> sprintf "%A" |> output.WriteLine

        sprintf "%A \n %A" a b |> output.WriteLine
        0
    [<Fact>]
    member __.``lisp``() =
        let term = R "term" "[^\(\)\:\s]+" =>
                   fun state ast ->
                   match ast with
                   | Token tk -> Value <| Term tk.value
                   | _ -> failwith "emmm"

        let space = R "space" "\s+"
        let sexpr = Named "sexpr"

        let state = State<sexpr>.inst()
        let (:=) = state.implement

        Named "space" := space  (** only for building auto lexer which contains `space` from grammar*)

        sexpr := Or [And[C"("; Rep(0, -1, sexpr).bind_to("sexpr"); C")"]; term]
                 =>
                 fun state ast ->
                 match state.ctx.TryGetValue "sexpr" with
                 | (false, _) -> ast
                 | (true, it) ->
                 match it with
                 | Nested lst ->
                    Seq.map
                    <| fun (Value it) -> it
                    <| lst
                    |> List.ofSeq
                    |> S
                    |> Value
                 | _ -> failwith "emmm"



        let bounds_map, lexer_factors =
            analyse analysis.crate
                    [for each in ["space";"sexpr";] -> each, state.lang.[each]]

        let tokens = lex None lexer_factors {filename = ""; text = "(add 1 (mul 2 3))"}
                     |> Seq.filter (fun it -> it.name <> "space")
                     |> Array.ofSeq

        let ast = parse sexpr tokens state
        output.WriteLine <| sprintf "%A" ast
        0
