﻿module Result
open AST 

type Status<'T> = M | U | F | LR of 'T array(** matched | unmatched | finished | left recursive **)

type Result<'T, 'E>(status: Status<'T>, content: 'E) = 
    member this.status    = status 
    member this.content   = content
    static member finished              = Result<'T, 'E>(F,  Unchecked.defaultof<'E>)
    static member unmatched             = Result<'T, 'E>(U,  Unchecked.defaultof<'E>)
    static member matched (content: 'E) = Result<'T, 'E>(M,   content)
    static member findLR  (cases)       = Result<'T, 'E>(LR cases, Unchecked.defaultof<'E>)