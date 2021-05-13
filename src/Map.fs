namespace Mimir.FSharp.Extensions

module Map =
    let tryFindAll (keys:'Key list) (map:Map<'Key,'Value>) =
        let values = keys |> List.choose (map.TryFind)
        (keys.Length = values.Length, values)
        |> Option.ofPair

    let update key updater (map:Map<'Key,'Value>) =
        let update = map |> Map.tryFind key |> updater
        match update with
        | None -> map
        | Some value -> map |> Map.add key value

    let inline addOrUpdate key (updater) (map:Map<'Key,'Value>) =
        map
        |> update key (updater >> Some)

    let addOrReplaceWith key getValue (map:Map<'Key,'Value>) =
        map
        |> update key (fun _ -> Some <| getValue())

    let upsert key insert (update) value (map:Map<'Key,'Value>) =
        match map |> Map.tryFind key with
        | None ->
            map |> Map.add key (insert value)
        | Some entry ->
            map |> Map.add key (update value entry)

    let merge (mapA : Map<'a, 'b>) (mapB : Map<'a, 'b>) (mergeValues : 'a -> 'b -> 'b -> 'b) =
        Map.fold (fun state key valueA ->
            match Map.tryFind key state with
            | Some valueB -> Map.add key (mergeValues key valueA valueB) state
            | None -> Map.add key valueA state) mapA mapB

    let inline singleton key value =
        Map.add key value Map.empty