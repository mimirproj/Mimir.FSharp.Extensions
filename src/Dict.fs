namespace Mimir.FSharp.Extensions

open System.Collections.Generic

type Dict<'TKey, 'TValue when 'TKey : equality> = IDictionary<'TKey, 'TValue>

module Dict =
    let empty<'TKey, 'TValue when 'TKey : equality> : IDictionary<'TKey, 'TValue> =
        dict []

    let inline tryFind key (dictionary:IDictionary<'TKey, 'TValue>) =
        dictionary.TryGetValue(key) |> Option.ofPair

    let tryFindAll (keys:'TKey list) (dictionary:IDictionary<'TKey, 'TValue>) =
        let values = keys |> List.choose(fun key -> dictionary |> tryFind key)
        (keys.Length = values.Length, values)
        |> Option.ofPair

    let update key updater (dictionary:IDictionary<'TKey, 'TValue>) =
        let update = dictionary |> tryFind key |> updater
        match update with
        | None -> ()
        | Some value -> dictionary.[key] <- value

    let inline addOrUpdate key (updater) (map:Dict<'Key,'Value>) =
        map
        |> update key (updater >> Some)

    let addOrReplaceWith key add replace (dictionary:IDictionary<'TKey, 'TValue>) =
        dictionary
        |> update key (
            function
            | None -> add() |> Some
            | Some item -> replace item |> Some)

[<AutoOpen>]
module DictPervasive =
    let inline dict (items:('Key * 'Value) seq) =
        let d = new System.Collections.Generic.Dictionary<'Key, 'Value>()
        items |> Seq.iter(d.Add)
        d

    type IDictionary<'TKey, 'TValue> with
        member self.TryFind(key) =
            self |> Dict.tryFind key

        member self.TryFindAll keys =
            self |> Dict.tryFindAll keys

        member self.Update(key, updater) =
            self |> Dict.update key updater

    let inline (|Key|_|) key dict =
        dict |> Dict.tryFind key

    let inline (|Keys|_|) keys dict =
        dict |> Dict.tryFindAll keys
