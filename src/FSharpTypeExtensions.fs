[<AutoOpen>]
module Mimir.FSharp.Extensions.FSharpTypeExtensions

open System
open System.Reflection
open FSharp.Reflection

type MemberAccessibility = Public | NonPublic

type FSharpType with
    static member inline IsSingleCaseUnion(typ:Type, ?accessibility:MemberAccessibility) =
        let bindingFlags =
            match accessibility with
            | Some Public -> BindingFlags.Public
            | Some NonPublic -> BindingFlags.NonPublic
            | None -> BindingFlags.Public ||| BindingFlags.NonPublic

        (FSharpType.GetUnionCases(typ, bindingFlags)).Length = 1

    static member inline IsSingleCaseUnion<'t>(?accessibility:MemberAccessibility) =
        FSharpType.IsSingleCaseUnion(typeof<'t>, ?accessibility=accessibility)