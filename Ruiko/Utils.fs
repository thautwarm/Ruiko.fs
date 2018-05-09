module Utils

open System

type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
        
type CStack<'e> = System.Collections.Generic.Stack<'e>

let range = System.Linq.Enumerable.Range
let ConstStrPool: CDict<string, string> = new CDict<string, string>()
let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (false, _) -> 
        ConstStrPool.Add(s, s)
        s
    
    | (true, r) -> r

let inline (&=) a b = obj.ReferenceEquals(a, b)

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

let inline bye'with (action: (unit -> unit)) (result: 'T)  = 
    action()
    result

type tuple =
    
    static member apply  (f1: 'T1 -> 'G, f2: 'T2 -> 'H) 
                         (arg1: 'T1, arg2: 'T2) = 
                         f1 arg1, f2 arg2

    static member apply3 (f1: 'T1 -> 'G, f2: 'T2 -> 'H, f3: 'T3 -> 'R)
                         (arg1: 'T1, arg2: 'T2, arg3: 'T3)  = 
                          f1 arg1, f2 arg2, f3 arg3
    

let NotImplemented = NotImplementedException()

let inline (</>) (arg: 'T list) (f: 'T -> 'G) = List.map f arg  

let inline (<*>) (arg: 'T) (f1: 'T -> 'G, f2: 'T -> 'H) = f1 arg, f2 arg

let inline (<**>) (arg: 'T) (f1: 'T -> 'G, f2: 'T -> 'H, f3: 'T -> 'R) = f1 arg, f2 arg, f3 arg
    
let inline (<..>)  (a: 'T) (b: 'G): 'T * 'G = a, b