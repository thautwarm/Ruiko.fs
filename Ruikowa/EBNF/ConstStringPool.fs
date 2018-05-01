module ConstStringPool
open RuikoCollections
let ConstStrPool: CDict<string, string> = new CDict<string, string>()
let Const'Cast (s: string) = 
    match ConstStrPool.TryGetValue s with
    | (true, _) -> 
        ConstStrPool.Add(s, s)
        s
    | (false, r) -> r