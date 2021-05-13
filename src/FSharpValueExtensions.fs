[<AutoOpen>]
module Mimir.FSharp.Extensions.FSharpValueExtensions

open System
open System.Reflection
open FSharp.Reflection

type FSharpValue with
    static member inline IsSingleCaseUnionCtor(ctor:'v -> 't, ?accessibility:MemberAccessibility) =
        FSharpType.IsSingleCaseUnion<'t>(?accessibility=accessibility)

    static member inline IsSingleCaseUnionValue(value:'v, ?accessibility:MemberAccessibility) =
        let typ = value.GetType()
        FSharpType.IsSingleCaseUnion(typ, ?accessibility=accessibility)