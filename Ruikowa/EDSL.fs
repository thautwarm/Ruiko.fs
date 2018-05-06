module EDSL

open Parser
open AST 
open Tokenizer
open Utils

type Term = 
    | R   of string
    | R'  of string
    
    | N   of string
    | N'  of string 

    | V   of string
    | V'  of string
    
    | C   of string
    | C'  of string

    | NC  of string * string

    | Fn  of (Tokenizer -> bool)

    | Seq of Term * int * int

    | As  of Term * string

    | P   of Term list list

    | Ref of string

    with 
        member this.(+)  = Seq(this, 1, -1)
        member this.(*)  = Seq(this, 0, -1)
        member this.(=>) (name: string) = As(this, name)
   

type Def(name: string, lexer: Lexer CList, lang: LanguageArea) =
    let s_name = name
    let mutable _restructure : (State -> AST) option = None
    let mutable _definition: Term list list =  []
    let mutable ``generated?`` = false

    let _when   = When()
    let _with   = With()
    let _lexer  = lexer
    let _lang   = lang
    
    

    
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

        
        member this.ToParser () = 
            raise NotImplemented

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
        




        

let def (lexerBuilder: Lexer CList) (language: LanguageArea) 
        (name: string) =
        Def(name, lexerBuilder, language)
        
    
    

  