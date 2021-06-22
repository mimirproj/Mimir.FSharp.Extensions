namespace Mimir.FSharp.Extensions

module Map =
    let tryFindAll (keys:'Key list) (map:Map<'Key,'Value>) =
        let values = keys |> List.choose (map.TryFind)
        (keys.Length = values.Length, values)
        |> Option.ofPair

    let updateWith key updater (map:Map<'Key,'Value>) =
        let update = map |> Map.tryFind key |> updater
        match update with
        | None -> map
        | Some value -> map |> Map.add key value

    let inline addOrUpdateWith key (updater) (map:Map<'Key,'Value>) =
        map
        |> updateWith key (updater >> Some)

    let merge (mapA : Map<'a, 'b>) (mapB : Map<'a, 'b>) (mergeValues : 'a -> 'b -> 'b -> 'b) =
        Map.fold (fun state key valueA ->
            match Map.tryFind key state with
            | Some valueB -> Map.add key (mergeValues key valueA valueB) state
            | None -> Map.add key valueA state) mapA mapB

    let inline singleton key value =
        Map.add key value Map.empty