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
| Empty_sexpr

type Expr =
| Add of left: Expr * right: Expr
| Sym of string
| Empty_expr

type RecurNode = 
| Node1     of RecurNode * string
| Node2     of RecurNode * string
| Node3     of RecurNode * string
| LitNode   of string 
| Empty_recur_node


type default_value = Default
    with static member copy this = this
type MyTests(output:ITestOutputHelper) =

    [<Fact>]
    member __.``Direct Left Recursion`` () =

        let v1 = V "123"
        let v2 = V "234"
        let node_name = "node"
        let node = Named(node_name, fun () -> Default)
        let node_impl = Or [And [node; v1]; v2]
        let tokens = def_token ["234"; "123"; "123"]
        let state = State<default_value>.inst(Default)
        let (:=) = state.implement
        node := node_impl
        parse node tokens state |> sprintf "%A" |> output.WriteLine
        0
  
    [<Fact>]
    member __.``recursive left recursion`` () =
        let v1 = C "a"
        let v2 = C "b"
        let v3 = C "c"
        let v4 = C "d"
        
        let node1 = Named("node1", fun () -> Node1(Empty_recur_node, ""))
        let node2 = Named("node2", fun () -> Node2(Empty_recur_node, ""))
        let node3 = Named("node3", fun () -> Node3(Empty_recur_node, ""))

        let state = State<RecurNode>.inst()
        let (:=) = state.implement

        node1 := Or [And [
                           node1 % fun (Node1(_, r)) (Value it) -> Node1(it, r)
                           v1    % fun (Node1(l, _)) (Token it) -> Node1(l, it.value)
                         ]
                     node2 % fun _ (Value it) -> it
                    ] => fun state _ -> state.context |> Value

        node2 := Or [And [
                          node2 % fun (Node2(_, r)) (Value it) -> Node2(it, r)
                          v2    % fun (Node2(l, _)) (Token it) -> Node2(l, it.value)
                          ]
                     node3 % fun _ (Value it) -> it
                    ] => fun state _ -> state.context |> Value
        
        
        node3 := Or [And [node3 % fun (Node3(_, r)) (Value it) -> Node3(it, r) 
                          v3    % fun (Node3(l, _)) (Token it) -> Node3(l, it.value)
                          ]; 
                     v4    % fun _ (Token t) -> LitNode t.value
                     ] => fun state _ -> state.context |> Value
        
        let lexer_tb = analyse [v1; v2; v3; v4]
        let tokens = lex None lexer_tb {filename = ""; text = "dccbaaa"} |> Array.ofSeq
        parse node1 tokens state |> sprintf "%A" |> output.WriteLine
        0

    [<Fact>]
    member __.``Indirect Left Recursion`` () =

        let v1 = V("a")
        let v2 = V("b")
        let v3 = V("c")
        let node = Named("node", fun () -> Default)
        let mid = Named("mid", fun () -> Default)

        let node_impl = Or [And [mid; v2]; v1]
        let mid_impl = And [node; v3]
        let tokens = def_token ["a"; "c"; "b"; "c"; "b"]

        let state = State<default_value>.inst(Default)
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
        let top = Add(Empty_expr, Empty_expr)
        let plus = Named(plus_name, fun () -> top)
        let plus_impl = 
            Or [
                And [
                    plus % fun (Add(_, r)) (Value l) -> Add(l, r)
                    plus_operator; 
                    identifier % fun (Add(l, _)) (Token it) -> Add(l, Sym(it.value))
                    ]
                identifier % fun _ (Token it) ->  Sym(it.value)
            ] => fun state _ -> Value <| state.context

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
        let state = State<Expr>.inst(top)
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
        let plus = Named("plus", fun () -> Add(Empty_expr, Empty_expr))

        let (:=) = state.implement
        let identifier =
            R "identifier" "[a-zA-Z_]{1}[a-zA-Z_0-9]*"

        plus := Or
                [
                    And[
                        plus
                            % fun (Add(_, b)) (Value it) -> Add(it, b) 
                        C "+"
                        identifier 
                            % fun (Add(a, _)) (Token it) -> Add(a, Sym(it.value))
                    ]
                    identifier 
                        % fun _ (Token it) -> Sym(it.value)
                ] => fun state _ -> state.context |> Value

        let lexer_tb = analyse [for each in ["plus"] -> state.lang.[each]]


        let tokens = lex None lexer_tb {filename=""; text="abs+abs+abs"} |> Array.ofSeq

        parse plus tokens state |> sprintf "%A" |> output.WriteLine

        sprintf "%A" lexer_tb |> output.WriteLine
        0
    [<Fact>]
    member __.``lisp``() =
        
        let term = R "term" "[^\(\)\:\s]+"
        let sexpr = Named("sexpr", fun () -> Empty_sexpr)

        let state = State<sexpr>.inst()
        let (:=) = state.implement
        sexpr := Or [
                    And[
                        C"("
                        Rep(0, -1, sexpr) 
                            % 
                            fun _ (Nested it) ->
                            Array.map
                            <| fun (Value it) -> it
                            <| it.ToArray()
                            |> List.ofArray
                            |> S
                        C")"
                    ]
                    term % fun _ (Token it) -> Term(it.value)
                ] => fun state _ -> Value <| state.context

        let lexer_factors =
            analyse [R "space" "\s+"; state.lang.["sexpr"]]

        let tokens = lex None lexer_factors {filename = ""; text = "(add 1 (mul 2 3))"}
                     |> Seq.filter (fun it -> it.name <> "space")
                     |> Array.ofSeq

        let ast = parse sexpr tokens state
        output.WriteLine <| sprintf "%A" ast
        0
