module RBNF.Infras


type ('k, 'v) hashmap = System.Collections.Generic.Dictionary<'k, 'v>
type 'T arraylist = System.Collections.Generic.List<'T>

type 'a M =
    | Just of 'a
    | Nothing

let (|>>) (m : 'a M) (f : 'a -> 'b M) : 'b M =
    match m with
    | Just a ->  f a
    | Nothing -> Nothing

let Return (a: 'a) : 'a M =
    Just a

let (&=) a b =
    obj.ReferenceEquals(a, b)


let mutable _log_started = false 


let inline Log msg = 
    #if Debug 
    if _log_started then
        System.IO.File.AppendAllText("../log", msg + "\n")
    else 
        _log_started <- true
        System.IO.File.WriteAllText("../log", msg + "\n")
    #else
    ()
    #endif 
    
