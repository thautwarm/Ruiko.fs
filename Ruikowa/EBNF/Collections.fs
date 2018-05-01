module RuikoCollections


type 'a CList = 'a System.Collections.Generic.List
type CDict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type ArrayView<'T> = {
    arr   : 'T array
    start :  int
}

