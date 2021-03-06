[<AutoOpen>]
module Mimir.FSharp.Extensions.Prelude

type 'a task = System.Threading.Tasks.Task<'a>

[<CompiledName("hole")>]
[<CompilerMessage("Unresolved type hole", 9999, IsError=false)>]
let inline ___<'a> =
    sprintf "There is a type hole of %s in your code!" (typeof<'a>).FullName
    |> failwith<'a>

/// An active pattern which returns the Length member of any instance which has it.
let inline (|HasLength|) v =
    (fun () -> (^a : (member Length: int) v))()

/// A constant function, a function that always returns the same value.
let inline konst k = fun _ -> k

/// Try convert the value to T, returning Some T if possible otherwise None.
let inline tryCast<'a> (value : obj) =
    match value with
    | :? 'a as res -> Some res
    | _ -> None


/// Compose two functions that return a bool by ORing the results.
let inline (>||) (fa:'a -> bool) (fb:'a -> bool) v = 
    fa v || fb v

/// Compose two functions that return a bool by ANDing. the results.
let inline (>&&) (fa:'a -> bool) (fb:'a -> bool) v = 
    fa v && fb v