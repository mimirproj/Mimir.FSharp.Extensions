module Mimir.FSharp.Extensions.String

open System

let indexOf (searchStr:string) (startIndex:int) (inputStr:string) =
    if isNull inputStr || isNull searchStr || startIndex < 0 || startIndex > inputStr.Length then
        None
    else
        let index = inputStr.IndexOf(searchStr, startIndex)
        if index >= 0 then Some index
        else None

let indexOfChar (searchChar:char) (startIndex:int) (inputStr:string) =
    if isNull inputStr || startIndex < 0 || startIndex > inputStr.Length then
        None
    else
        let index = inputStr.IndexOf(searchChar, startIndex)
        if index >= 0 then Some index
        else None

let indicesOf (searchStr:string) (inputStr:string) =
    let indexOf startIndex = indexOf searchStr startIndex inputStr

    indexOf 0
    |> Array.unfold(Option.map (fun lastIndex -> (lastIndex, indexOf(lastIndex + 1))))

let indicesOfChar (searchChar:char) (inputStr:string) =
    let indexOf startIndex = indexOfChar searchChar startIndex inputStr

    indexOf 0
    |> Array.unfold(Option.map (fun lastIndex -> (lastIndex, indexOf(lastIndex + 1))))
