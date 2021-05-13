namespace Mimir.FSharp.Extensions

open System

module Option =
    /// Create an Option<'T> from a tuple-2.
    let inline ofPair(success : bool, value) =
        if success then Some value
        else None