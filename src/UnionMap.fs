namespace Mimir.FSharp.Extensions

open System
open System.Reflection
open FSharp.Reflection

type UnionMap<'union, 'assoc> =
    private {
        GetUnionCaseInfo: 'union -> UnionCaseInfo

        /// This contains the actual argument value (and an associated value) that is used to
        /// construct a union value, given the union case tag.
        CaseArgumentByCaseTag: Map<int, {| BoxedArgValue: obj; AssociatedValue: 'assoc |}>

        /// This contains a sample argument value that can be used to
        /// construct a union value, given the case constructor.
        CaseArgumentSampleByTypeName: Map<string, {| BoxedArgSample: obj; RefCount: uint32 |}>
    }


[<RequireQualifiedAccess>]
module UnionMap =
    [<GeneralizableValue>]
    let empty<'union, 'assoc> : UnionMap<'union, 'assoc> =

        let getUnionCaseInfo (unionValue:'union) =
            FSharpValue.GetUnionFields (unionValue, typeof<'union>, BindingFlags.Public ||| BindingFlags.NonPublic)
            |> fst

        { GetUnionCaseInfo = getUnionCaseInfo
          CaseArgumentByCaseTag = Map.empty
          CaseArgumentSampleByTypeName = Map.empty
        }

    let tryFind (caseCtor:'caseArg -> 'union)
                (unionMap:UnionMap<'union, 'assoc>)
                : ('caseArg * 'assoc) option =

        let anyUnionCaseInfo =
            unionMap.CaseArgumentSampleByTypeName
            |> Map.tryFind (typeof<'caseArg>).FullName
            |> Option.bind (fun entry -> tryCast<'caseArg> entry.BoxedArgSample)
            |> Option.map (caseCtor >> unionMap.GetUnionCaseInfo)

        anyUnionCaseInfo
        |> Option.bind (fun unionCaseInfo ->
            unionMap.CaseArgumentByCaseTag
            |> Map.tryFind unionCaseInfo.Tag)
        |> Option.bind (fun entry ->
            tryCast<'caseArg> entry.BoxedArgValue
            |> Option.map (fun arg -> (arg, entry.AssociatedValue))
        )

    let exists (caseCtor:'caseArg -> 'union)
               (predicate: 'caseArg -> 'assoc -> bool)
               (unionMap:UnionMap<'union, 'assoc>) =

        unionMap
        |> tryFind caseCtor
        |> Option.exists (fun (ctorInput, associatedValue) -> predicate ctorInput associatedValue)


    let inline contains (caseCtor:'caseArg -> 'union)
                        (unionMap:UnionMap<'union, 'assoc>) =

        exists caseCtor (fun _ _ -> true) unionMap


    let add (caseCtor:'caseArg -> 'union)
            (caseArgument:'caseArg)
            (associatedValue:'assoc)
            (unionMap:UnionMap<'union, 'assoc>)
            : UnionMap<'union, 'assoc>  =

        let unionValue = caseCtor caseArgument
        let unionCaseInfo = unionMap.GetUnionCaseInfo unionValue

        { unionMap with
            CaseArgumentByCaseTag =
                unionMap.CaseArgumentByCaseTag
                |> Map.addOrUpdateWith
                    unionCaseInfo.Tag
                    (fun _ -> // Always replaces the existing entry.
                        {| BoxedArgValue = box caseArgument
                           AssociatedValue = associatedValue
                        |}
                    )

            CaseArgumentSampleByTypeName =
                unionMap.CaseArgumentSampleByTypeName
                |> Map.addOrUpdateWith
                    ((typeof<'caseArg>).FullName)
                    (Option.map(fun entry ->
                        {| entry with
                            RefCount =
                                if unionMap.CaseArgumentByCaseTag.ContainsKey unionCaseInfo.Tag then
                                    entry.RefCount
                                else
                                    entry.RefCount + 1ul
                        |})
                     >> Option.defaultValue
                        {| RefCount = 1ul
                           BoxedArgSample = box caseArgument
                        |}
                    )
        }


    let inline singleton (caseCtor:'caseArg -> 'union)
                         (caseArgument:'caseArg)
                         (associatedValue:'assoc) =

        empty<'union, 'assoc>
        |> add caseCtor caseArgument associatedValue


    let remove (caseCtor:'caseArg -> 'union)
               (unionMap:UnionMap<'union, 'assoc>) =

        let caseArgTypeName = (typeof<'caseArg>).FullName

        let anyCaseArgumentSample =
            unionMap.CaseArgumentSampleByTypeName
            |> Map.tryFind caseArgTypeName

        let anyUpdatedCaseArgumentByCaseTag =
            anyCaseArgumentSample
            |> Option.bind (fun entry -> tryCast<'caseArg> entry.BoxedArgSample)
            |> Option.map (caseCtor >> unionMap.GetUnionCaseInfo)
            |> Option.bind (fun unionCaseInfo ->
                if unionMap.CaseArgumentByCaseTag.ContainsKey unionCaseInfo.Tag then
                    unionMap.CaseArgumentByCaseTag
                    |> Map.remove unionCaseInfo.Tag
                    |> Some

                else
                    None
            )

        (anyCaseArgumentSample, anyUpdatedCaseArgumentByCaseTag)
        ||> Option.map2(fun caseArgumentSample updatedCaseArgumentByCaseTag  ->
            { unionMap with
                CaseArgumentByCaseTag =
                    updatedCaseArgumentByCaseTag

                CaseArgumentSampleByTypeName =
                    if caseArgumentSample.RefCount < 2ul then
                        unionMap.CaseArgumentSampleByTypeName
                        |> Map.remove caseArgTypeName

                    else
                        unionMap.CaseArgumentSampleByTypeName
                        |> Map.add
                            caseArgTypeName
                            {| caseArgumentSample with RefCount = caseArgumentSample.RefCount - 1ul |}
            })
        |> Option.defaultValue unionMap


    let updateWith (caseCtor:'caseArg -> 'union)
                   (updater: Option<'caseArg * 'assoc> -> Option<'caseArg * 'assoc>)
                   (unionMap:UnionMap<'union, 'assoc>) =

        let update =
            unionMap
            |> tryFind caseCtor
            |> updater

        match update with
        | None -> unionMap
        | Some (updatedCaseArg, updatedAssociatedValue) ->
            unionMap
            |> add caseCtor updatedCaseArg updatedAssociatedValue

    let inline addOrUpdateWith (caseCtor:'caseArg -> 'union)
                               updater
                               (unionMap:UnionMap<'union, 'assoc>) =

        unionMap
        |> updateWith caseCtor (updater >> Some)