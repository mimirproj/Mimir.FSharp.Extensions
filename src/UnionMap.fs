namespace Mimir.FSharp.Extensions

open System
open System.Reflection
open FSharp.Reflection

type UnionMap<'union, 'associatedValue> =
    private {
        GetUnionInfo: 'union -> FSharp.Reflection.UnionCaseInfo
        Map: Map<string,
                 {| ConstructorInputExample: obj
                    ConstructorInputByTag: Map<int, obj * 'associatedValue>
                 |}>
    }

    with
        member this.IsEmpty =
            this.Map.IsEmpty


module UnionMap =
    [<GeneralizableValue>]
    let empty<'union, 'associatedValue> : UnionMap<'union, 'associatedValue> =
        let unionType = typeof<'union>

        { Map = Map.empty
          GetUnionInfo = fun union ->
            FSharpValue.GetUnionFields(union, unionType, BindingFlags.Public ||| BindingFlags.NonPublic)
            |> fst
        }

    let add (ctor:'ctorInput -> 'union)
            (ctorInput:'ctorInput)
            (associatedValue:'associatedValue)
            (unionMap:UnionMap<'union, 'associatedValue>) =

        let union = ctor ctorInput
        let unionInfo = unionMap.GetUnionInfo union

        { unionMap with
            Map =
                unionMap.Map
                |> Map.addOrUpdateWith (typeof<'ctorInput>.Name)
                    (Option.map(fun existing ->
                        {| existing with
                            ConstructorInputByTag =
                                existing.ConstructorInputByTag
                                |> Map.add unionInfo.Tag (box ctorInput, associatedValue)
                        |})

                     >> Option.defaultWith(fun _ ->
                        {| ConstructorInputExample = ctorInput
                           ConstructorInputByTag = Map.ofList [ unionInfo.Tag, (box ctorInput, associatedValue) ]
                        |})
                    )

        }


    let tryFind (ctor:'ctorInput -> 'union)
                (unionMap:UnionMap<'union, 'associatedValue>) =

        unionMap.Map
        |> Map.tryFind (typeof<'ctorInput>.Name)
        |> Option.bind(fun entry ->
            let unionInfo =
                ctor (downcast entry.ConstructorInputExample)
                |> unionMap.GetUnionInfo

            entry.ConstructorInputByTag
            |> Map.tryFind unionInfo.Tag
            |> Option.map(fun (boxedCtorInput, associatedValue) ->
                ( boxedCtorInput :?> 'ctorInput
                , associatedValue
                )
            )
        )


    let exists (ctor:'ctorInput -> 'union)
               (predicate: 'ctorInput -> 'associatedValue -> bool)
               (unionMap:UnionMap<'union, 'associatedValue>) =

        unionMap
        |> tryFind ctor
        |> Option.exists(fun (ctorInput, associatedValue) -> predicate ctorInput associatedValue)


    let containsKey (ctor:'ctorInput -> 'union)
                    (unionMap:UnionMap<'union, 'associatedValue>) =

        unionMap.Map
        |> Map.tryFind (typeof<'ctorInput>.Name)
        |> Option.exists(fun entry ->
            let unionInfo =
                ctor (downcast entry.ConstructorInputExample)
                |> unionMap.GetUnionInfo

            entry.ConstructorInputByTag
            |> Map.containsKey unionInfo.Tag)


    let remove (ctor:'ctorInput -> 'union)
               (unionMap:UnionMap<'union, 'associatedValue>) =

        { unionMap with
            Map =
                unionMap.Map
                |> Map.updateWith (typeof<'ctorInput>.Name)
                    (Option.bind(fun entry ->
                        let unionInfo =
                            ctor (downcast entry.ConstructorInputExample)
                            |> unionMap.GetUnionInfo

                        let nextConstructorInputByTag =
                            entry.ConstructorInputByTag
                            |> Map.remove unionInfo.Tag

                        if nextConstructorInputByTag.IsEmpty then
                            None

                        else
                            Some
                                {| entry with
                                    ConstructorInputByTag = nextConstructorInputByTag
                                |}
                        )
                    )
        }

    let updateWith (ctor:'ctorInput -> 'union)
                   updater
                   (unionMap:UnionMap<'union, 'associatedValue>) =

        let update = unionMap |> tryFind ctor |> updater
        match update with
        | None -> unionMap
        | Some (ctorInput, assocValue) ->
            unionMap
            |> add ctor ctorInput assocValue

    let inline addOrUpdate (ctor:'ctorInput -> 'union)
                           updater
                           (unionMap:UnionMap<'union, 'associatedValue>) =

        unionMap
        |> updateWith ctor (updater >> Some)

    let inline addOrUpdateWith (ctor:'ctorInput -> 'union)
                               (updater)
                               (unionMap:UnionMap<'union, 'associatedValue>) =

        unionMap
        |> updateWith ctor (updater >> Some)