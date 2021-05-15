module Mimir.FSharp.Extensions.Result

let inline mapBoth (forOk : 'a -> 'c) (forError : 'b -> 'c) result =
    match result with
    | Ok v -> forOk v
    | Error e -> forError e

let inline isOk result =
    result
    |> mapBoth (konst true) (konst false)

let inline isError result =
    result
    |> mapBoth (konst false) (konst true)

let inline asOk result =
    result
    |> mapBoth Some (konst None)

let inline asError result =
    result
    |> mapBoth (konst None) Some

let inline orElse (next : Result<'a,'b>) (result : Result<'a,'b>) =
    result
    |> mapBoth Ok (konst next)

let ofOption (error: 'b) option =
    option
    |> Option.mapBoth Ok (fun _ -> Error error)

let inline toOption result =
    result |> asOk