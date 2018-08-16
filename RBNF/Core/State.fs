module RBNF.State
open RBNF.Infras
open RBNF.AST
open Ruikowa.CSharp

type ('P, 'T) GuardRewriter = {
    parser: 'P
    enter: (Token array -> ('P, 'T) State  -> bool) option
    exit : (Token array -> ('P, 'T) State  -> bool) option
    rewrite : (('P, 'T) State -> 'T) option
}
and ('P, 'T) State = {
    mutable lr    : string option
    mutable ctx   : (string, 'T AST) hashmap
    trace : string Trace Trace
    lang  : (string, ('P, 'T) GuardRewriter) hashmap
    }
    with
    member this.end_index with get() = this.trace.EndIndex

    member this.max_fetched with get() = this.trace.MaxFetched
    member this.current with get () = this.trace.[this.trace.EndIndex]
    member this.reset(history) =
            let (base', branch, ctx) = history
            this.ctx <- ctx
            this.trace.Reset(base')
            this.current.Reset(branch)

    member this.new_one() =
        if this.trace.Inc(fun () -> Trace<string>()) = false then
            this.current.Clear()
        else
            ()


    member this.append(e : string) =
        this.current.Append(e)
    member this.commit() =
        (this.trace.Commit(), this.current.Commit(), this.ctx)

    member this.contains(record : string) =
        this.current.Contains(record)

let left_recur (self : ('P, 'V) State) (lr_name : string) (fn : ('P, 'V) State -> 'R) : 'R =
    self.lr <- Some(lr_name)
    let ret = fn(self)
    in
    self.lr <- None
    ret

let with_context_recovery (self: ('P, 'V) State) (fn : ('P, 'V) State -> 'R) =
    let ctx = self.ctx
    let ret = fn(self)
    in
    self.ctx <- ctx
    ret
