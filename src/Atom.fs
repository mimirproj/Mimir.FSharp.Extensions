namespace Mimir.FSharp.Extensions

open System
open System.Threading

[<Sealed>]
type Atom<'T when 'T : not struct>(value : 'T, ?spinDuration) =
    let refCell = ref value

    let rec swap f =
        let currentValue = !refCell
        let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then result
        else
            Thread.SpinWait(defaultArg spinDuration 10)
            swap f

    member __.Value =
        !refCell

    member __.Swap(f : 'T -> 'T) =
        swap f

[<RequireQualifiedAccess>]
module Atom =
    let inline create value = Atom<_>(value)
    let inline swap (f : _ -> _) (atom : Atom<_>) = atom.Swap f
