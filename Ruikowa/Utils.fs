module Utils

open System

type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let range = System.Linq.Enumerable.Range
let ConstStrPool: CDict<string, string> = new CDict<string, string>()
let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (false, _) -> 
        ConstStrPool.Add(s, s)
        s
    
    | (true, r) -> r

let inline (&=) a b  = obj.ReferenceEquals(a, b)

let inline (&!=) a b = not (a &= b)

let inline by'the'way(action: unit -> 'T): bool = 
    action() |> ignore
    true

let inline SMonad (``do``: 'T -> 'G) (from: 'T option) =
    match from with 
    | None       -> None
    | Some from' -> ``do`` from' |> Some

let inline BMonad (``do``: unit-> 'G) (from: bool) =
    match from with 
    | false       -> Unchecked.defaultof<'G>
    | _           -> ``do``()

let inline BMonad' (``do``: unit-> 'G) (from: bool) (default': 'G) =
    match from with 
    | false       -> default'
    | _           -> ``do``()



let NotImplemented = NotImplementedException()

let inline (<*>) (arg: 'T)   (f1: 'T -> 'G, f2: 'T -> 'H) = f1 arg, f2 arg
    
let inline (<..>)  (a: 'T) (b: 'G): 'T * 'G = a, b
let inline (<...>) (a: 'T) (b: 'G) (c: 'R): 'T * 'G * 'R = a, b, c

let inline (<=??=>) (arg: 'T option *  (unit ->'G)) (f: 'T -> 'G) =
    match arg with 
    | (None, default') ->  default'()
    | (r,    _)        ->  f(r.Value)

let inline (=??=>) (arg: 'T option *  ('G)) (f: 'T -> 'G) =
    match arg with 
    | (None, default') ->  default'
    | (r,    _)        ->  f(r.Value)

let inline (<=?=>) (arg: bool *  (unit ->'G)) (f: unit -> 'G) =
    match arg with 
    | (false, default') ->  default'()
    | (_,    _)        ->   f()

let inline (=?=>) (arg: bool * 'G) (f: unit -> 'G) =
    match arg with 
    | (false, default') ->  default'
    | (_,    _)        ->   f()

let inline get (any: 'T option) = any.Value







