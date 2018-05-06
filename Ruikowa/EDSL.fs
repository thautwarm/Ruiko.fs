module EDSL

open Parser
open AST 
open Tokenizer
open Utils
open System.Linq
open System.Text.RegularExpressions

type Term = 
    | R   of string
    | R'  of string
    
    | N   of string
    | N'  of string 

    | V   of string
    | V'  of string
    
    | C   of string
    | C'  of string

    | NC     of string * string

    | Fn     of (Tokenizer -> bool)

    | ``P|`` of (Term list) 

    | Seq of Term * int * int

    | As  of Term * string

    | P   of Term list list

    | Ref of string

    with 
        member this.(+)  = Seq(this, 1, -1)
        member this.(*)  = Seq(this, 0, -1)
        member this.(=>) (name: string) = As(this, name)
   
type NamedParserCollection = CDict<string, Named>


type Def(name: string, lexer: Lexer CList, lang: LanguageArea) =
    let mutable _restructure : (State -> AST) option = None
    let mutable _definition: Term list list =  []
    let mutable ``generated?`` = false
    let mutable _parser : Named option = None

    let _when   = When()
    let _with   = With()
    let _lexer  = lexer

    
    
    with

        member this.restructure 
            with set value = _restructure <- value 
            and  get ()    = _restructure

        member this.definition 
            with set value = _definition <- value
            and  get ()    = _definition

        member this.when' 
            with  get ()    = _when

        member this.with' 
            with  get ()    = _with

        


        static member By (def': Term list list) (subject: Def) = 
            subject.definition <- def'
            subject
        

        static member When  (fns: (string * (State -> bool)) list) (subject: Def) = 
            match fns with 
            | [] -> subject 
            | (name, fn):: xs ->
                subject.when'.Add (name, fn)
                Def.When xs subject
         
        static member With  (fns: (string * (State -> AST -> bool)) list) (subject: Def) = 
            match fns with 
            | [] -> subject 
            | (name, fn):: xs ->
                subject.with'.Add (name, fn)
                Def.With xs subject
        static member Restructure (fn: (State -> AST)) (subject: Def) = 
            subject.restructure <- Some(fn)
            subject
        
        
        member this.ToParser () = 
            if ``generated?`` 
            then 
                _parser.Value
            else 
            _parser <- Named("name", _when, _with, lang, _restructure) |> Some

            let rec for'each'atom (ret: Atom list) (cases: Term list) = 
                match cases with 
                | [] -> ret |> (List.rev >> And)
                | x :: xs -> 
                    match x with 
                    | R str         ->
                        Regex ("\G"+str) |> RegExp |> Literal |> ``Literal Atom``
                    | R' str        ->
                        Regex ("\G"+str) |> ``Not RegExp`` |> Literal |> ``Literal Atom``
                    | N name        ->
                        name             |> Name |> Literal |> ``Literal Atom``
                    | N' name       ->
                        name             |> ``Not Name`` |> Literal |> ``Literal Atom``
                    | V  str        ->
                        str              |> ValueStr |> Literal |> ``Literal Atom``
                    | V' str        ->
                        str              |> ``Not ValueStr`` |> Literal |> ``Literal Atom``
                    | C str         ->
                        str              |> Const'Cast |> ConstStr |> Literal |> ``Literal Atom``
                    | C' str        ->
                        str              |> Const'Cast |> ConstStr |> Literal |> ``Literal Atom``
                    | NC(name, str) ->
                        (name |> Const'Cast, str |> Const'Cast)
                        |> ``Name and ConstStr`` |>  Literal |> ``Literal Atom``
                    | Fn(fn) ->
                        fn |> ``Func Predicate`` |> Literal |> ``Literal Atom``
                    | Seq(term, least, most) ->
                        raise NotImplemented

                    
                    |> Atom
                    |> 
                    function 
                    | atom -> for'each'atom (atom::ret) xs 

                
            let rec for'each'and (ret: And list) (cases: Term list list) = 
                match cases with
                | []     -> ret |> List.rev

                | x :: xs ->
                    
                    for'each'and (for'each'atom [] x::ret) xs 
               


            
            raise NotImplemented




        
 
let rec ``Term -> Atom`` (definitions: NamedParserCollection) (x: Term) = 
    match x with 
    | R str         ->
        Regex ("\G"+str) |> RegExp |> Literal |> ``Literal Atom``
    | R' str        ->
        Regex ("\G"+str) |> ``Not RegExp`` |> Literal |> ``Literal Atom``
    | N name        ->
        name             |> Name |> Literal |> ``Literal Atom``
    | N' name       ->
        name             |> ``Not Name`` |> Literal |> ``Literal Atom``
    | V  str        ->
        str              |> ValueStr |> Literal |> ``Literal Atom``
    | V' str        ->
        str              |> ``Not ValueStr`` |> Literal |> ``Literal Atom``
    | C str         ->
        str              |> Const'Cast |> ConstStr |> Literal |> ``Literal Atom``
    | C' str        ->
        str              |> Const'Cast |> ConstStr |> Literal |> ``Literal Atom``
    | NC(name, str) ->
        (name |> Const'Cast, str |> Const'Cast)
        |> ``Name and ConstStr`` |>  Literal |> ``Literal Atom``
    | Fn(fn) ->
        fn |> ``Func Predicate`` |> Literal |> ``Literal Atom``
    | Seq(term, least, most) ->
        ``SubSequence Atom``(``Term -> Atom`` definitions term, least, most)
    
    | As(term, name) ->
        ``Term -> Atom`` definitions term |> fun it -> ``Name Binding Atom``(name, it)
    
    | Ref name ->
        definitions.[name] |> ``Named Atom``
    
    | ``P|`` branches ->
        branches 
        |> List.map (fun it -> [it])

        |> List.map (``Term -> Atom`` definitions) 
        |> And
        |> fun it -> Or [it] |> 


        raise NotImplemented

    |> Atom



//let def (lexerBuilder: Lexer CList) (language: LanguageArea) 
//        (name: string) =
//        Def(name, lexerBuilder, language)
        
    
    

  