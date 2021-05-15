namespace Mimir.FSharp.Extensions

open System

module Option =
    let inline ofPair(success : bool, value) =
        if success then Some value
        else None

    let inline bind2 continuation optionA optionB =
        match optionA, optionB with
        | Some a, Some b -> continuation a b
        | _, _ -> None

    let inline mapBoth (forSome : 'a -> 'b) (forNone : unit -> 'b) (option : Option<'a>) =
        match option with
        | None -> forNone()
        | Some value -> forSome value

