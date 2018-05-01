module Utils

let inline (&=) a b  = obj.ReferenceEquals(a, b)
let inline (&!=) a b = not (a &= b)