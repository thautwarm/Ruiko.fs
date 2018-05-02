module Utils

open System

type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let range = System.Linq.Enumerable.Range
let ConstStrPool: CDict<string, string> = new CDict<string, string>()
let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (true, _) -> 
        ConstStrPool.Add(s, s)
        s
    | (false, r) -> r

let inline (&=) a b  = obj.ReferenceEquals(a, b)

let inline (&!=) a b = not (a &= b)

let inline by'the'way(action: unit -> 'T): bool = 
    action() |> ignore
    true

let inline Monad (``do``: 'T -> 'G) (from: 'T option) =
    match from with 
    | None       -> None
    | Some from' -> ``do`` from' |> Some

let NotImplemented = NotImplementedException()

let inline (<*>) (arg: 'T) (f1: 'T -> 'G, f2: 'T -> 'H) = f1 arg, f2 arg




