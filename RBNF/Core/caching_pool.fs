module RBNF.CachingPool
open RBNF.Infras

let caching_pool : (string, string) hashmap = hashmap()

let cast str =
    match caching_pool.TryGetValue str with
        | (false, _) ->
            caching_pool.Add(str, str)
            str

        | (true, r) -> r
